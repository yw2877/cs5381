# functions.R

# This script contains functions used for multi-agent orchestration in R.

# Return a clearer error than a bare HTTP 404 when the Ollama model is missing.
chat_with_diagnostics = function(model, messages, output = "text", tools = NULL) {
  tryCatch(
    {
      if (is.null(tools)) {
        ollamar::chat(model = model, messages = messages, output = output, stream = FALSE)
      } else {
        ollamar::chat(model = model, messages = messages, tools = tools, output = output, stream = FALSE)
      }
    },
    error = function(e) {
      msg = conditionMessage(e)
      if (grepl("HTTP 404", msg, fixed = TRUE)) {
        host = Sys.getenv("OLLAMA_HOST")
        if (identical(host, "")) {
          host = "http://127.0.0.1:11434"
        }
        stop(
          paste0(
            "Ollama returned HTTP 404 for model '", model, "' at ", host, ". ",
            "This usually means the model is not installed on that Ollama server. ",
            "Run ensure_ollama_model('", model, "') before agent_run(), or pull it manually with ollamar::pull()."
          ),
          call. = FALSE
        )
      }
      stop(e)
    }
  )
}

# Check that Ollama is reachable and that the requested model exists locally.
ensure_ollama_model = function(model, auto_pull = TRUE) {
  host = Sys.getenv("OLLAMA_HOST")
  if (identical(host, "")) {
    host = "http://127.0.0.1:11434"
  }

  models = tryCatch(
    ollamar::list_models(),
    error = function(e) {
      stop(
        paste0(
          "Could not reach Ollama at ", host, ". ",
          "Start the Ollama server or set OLLAMA_HOST correctly. Original error: ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )

  model_names = if ("name" %in% names(models)) models$name else character()
  has_model = any(model_names == model)
  if (has_model) {
    return(invisible(model))
  }

  if (!auto_pull) {
    installed = if (length(model_names) > 0) paste(model_names, collapse = ", ") else "<none>"
    stop(
      paste0(
        "Ollama model '", model, "' is not installed on ", host, ". Installed models: ", installed
      ),
      call. = FALSE
    )
  }

  message("Ollama model '", model, "' is not installed. Pulling it now...")
  tryCatch(
    {
      ollamar::pull(model)
      invisible(model)
    },
    error = function(e) {
      stop(
        paste0(
          "Failed to pull Ollama model '", model, "'. ",
          "Original error: ", conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}

object_as_text = function(x) {
  trim_text = function(text, max_chars = 8000) {
    if (nchar(text) <= max_chars) {
      return(text)
    }
    paste0(substr(text, 1, max_chars), "\n\n[truncated for prompt length]")
  }

  if (is.null(x)) {
    return("<null>")
  }

  if (is.data.frame(x)) {
    return(trim_text(df_as_text(x)))
  }

  if (is.atomic(x) && length(x) == 1) {
    return(as.character(x))
  }

  json_attempt = tryCatch(
    jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    error = function(e) NULL
  )
  if (!is.null(json_attempt)) {
    return(trim_text(as.character(json_attempt)))
  }

  trim_text(paste(utils::capture.output(print(x)), collapse = "\n"))
}

execute_tool_call = function(tool_call) {
  tryCatch(
    do.call(tool_call$name, tool_call$arguments),
    error = function(e) {
      list(
        error = TRUE,
        tool = tool_call$name,
        message = conditionMessage(e)
      )
    }
  )
}

tool_calls_as_text = function(tool_calls) {
  if (length(tool_calls) == 0) {
    return("No tool calls were made.")
  }

  blocks = lapply(seq_along(tool_calls), function(i) {
    tool_call = tool_calls[[i]]
    arg_text = object_as_text(tool_call$arguments)
    out_text = object_as_text(tool_call$output)
    paste(
      paste0("Tool ", i, ": ", tool_call$name),
      "Arguments:",
      arg_text,
      "Output:",
      out_text,
      sep = "\n"
    )
  })

  paste(blocks, collapse = "\n\n")
}

#' @name agent
#' @title Agent Wrapper Function
#' @description A helper wrapper function that will run a single agent, with or without tools.
#' @param messages A list of messages to be sent to the agent. Must be created using \code{ollamar::create_message()}.
#' @param model The model to be used for the agent.
#' @param output The output format to be used for the agent. Options are "text", "jsonlist", "tools", "df", and more. See \code{ollamar::chat} for more options.
#' @param tools A list of tools metadata to be used for the agent.
#' @param all If TRUE, return all responses from the agent (eg. all values of the tool call result list.) If FALSE, return only the last response.
#' @note If the agent has tools, those tools must be named as objects in the global environment.
#' @note If tools are provided and \code{output != "tools"}, the agent performs a tool-selection pass, executes the tools in R, then runs a second pass that synthesizes a final answer from the tool results.
#' @return A list of responses from the agent.
#' @export
agent = function(messages, model = "smollm2:1.7b", output = "text", tools = NULL, all = FALSE) {
  if (is.null(tools)) {
    return(chat_with_diagnostics(model = model, messages = messages, output = output))
  }

  tool_calls = chat_with_diagnostics(
    model = model,
    messages = messages,
    tools = tools,
    output = "tools"
  )

  n_calls = length(tool_calls)
  if (n_calls > 0) {
    for (i in seq_len(n_calls)) {
      tool_calls[[i]]$output = execute_tool_call(tool_calls[[i]])
    }
  }

  if (all || identical(output, "tools")) {
    return(tool_calls)
  }

  if (n_calls == 0) {
    stop(
      paste0(
        "The model did not request any tool calls for a tool-enabled agent. ",
        "Try a more capable model or tighten the agent prompt."
      ),
      call. = FALSE
    )
  }

  followup_messages = c(
    messages,
    ollamar::create_message(
      role = "user",
      content = paste(
        "Tool results are now available.",
        "Use only these tool results plus the original request to produce the final answer.",
        "If a tool output includes an error, acknowledge the limitation instead of inventing a result.",
        "",
        tool_calls_as_text(tool_calls),
        sep = "\n"
      )
    )
  )

  chat_with_diagnostics(model = model, messages = followup_messages, output = output)
}

agent_run = function(role, task, tools = NULL, output = "text", model = MODEL) {
  messages = ollamar::create_messages(
    ollamar::create_message(role = "system", content = role),
    ollamar::create_message(role = "user", content = task)
  )

  agent(messages = messages, model = model, output = output, tools = tools)
}


#' @name df_as_text
#' @title Convert a data.frame to a text string
#' @description Converts a data.frame to a text string using knitr::kable().
#' @param df The data.frame to convert to a text string.
#' @return A text string.
#' @export
df_as_text = function(df) {
  tab = knitr::kable(df, format = "markdown")
  tab = as.character(tab)
  paste0(tab, collapse = "\n")
}
