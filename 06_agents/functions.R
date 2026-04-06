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

#' @name agent
#' @title Agent Wrapper Function
#' @description A helper wrapper function that will run a single agent, with or without tools.
#' @param messages A list of messages to be sent to the agent. Must be created using \code{ollamar::create_message()}.
#' @param model The model to be used for the agent.
#' @param output The output format to be used for the agent. Options are "text", "jsonlist", "tools", "df", and more. See \code{ollamar::chat} for more options.
#' @param tools A list of tools metadata to be used for the agent.
#' @param all If TRUE, return all responses from the agent (eg. all values of the tool call result list.) If FALSE, return only the last response.
#' @note If the agent has tools, those tools must be named as objects in the **global environment**.
#' @note If the agent has tools, perform a tool call.
#' @note If the agent has NO tools, perform a standard chat.
#' @importFrom ollamar chat
#' @return A list of responses from the agent.
#' @export 
agent = function(messages, model = "smollm2:1.7b", output = "text", tools = NULL, all = FALSE){

    # # Testing values
    # messages = create_message(role = "user", content = "Add 3 + 5.")
    # # Define a function to be used as a tool
    # add_two_numbers = function(x, y){
    #     return(x + y)
    # }

    # # Define the tool metadata as a list
    # tool_add_two_numbers = list(
    #     type = "function",
    #     "function" = list(
    #         name = "add_two_numbers",
    #         description = "Add two numbers",
    #         parameters = list(
    #             type = "object",
    #             required = list("x", "y"),
    #             properties = list(
    #                 x = list(type = "numeric", description = "first number"),
    #                 y = list(type = "numeric", description = "second number")
    #             )
    #         )
    #     )
    # )
    # tools = list(tool_add_two_numbers); 
    # model = "smollm2:1.7b"; 
    # output = "tools";

    # If the agent has NO tools, perform a standard chat
    if(is.null(tools)) {
        resp = chat_with_diagnostics(model = model, messages = messages, output = output)
        return(resp)
    } else {
        
    # If the agent has any tools, perform a tool call
    resp = chat_with_diagnostics(model = model, messages = messages, tools = tools, output = output)

    # For any given tool call, execute the tool call
    n_resp = length(resp)
    if(n_resp > 0){
    for(i in 1:n_resp) {
    # i = 1
    # Save the result of the tool call in an 'output' field 
    resp[[i]]$output = do.call(resp[[i]]$name, resp[[i]]$arguments)
    }
    }
    if(all) { return(resp) } else { return(resp[[n_resp]]$output) }
    }

}

agent_run = function(role, task, tools = NULL, output = "text",model = MODEL){
  # Testing values
  # role = role2; task = df_as_text(result1); 
  # tools = NULL; output = "text"; model = MODEL;

  # Define the messages to be sent to the agent
  messages = ollamar::create_messages(
    ollamar::create_message(role = "system", content = role),
    ollamar::create_message(role = "user", content = task)
  )

  # Run the agent
  resp = agent(messages = messages, model = model, output = output, tools = tools)
  return(resp)

}


#' @name df_as_text
#' @title Convert a data.frame to a text string
#' @description Converts a data.frame to a text string using knitr::kable().
#' @param df The data.frame to convert to a text string.
#' @return A text string.
#' @export
df_as_text = function(df){
  tab = knitr::kable(df, format = "markdown")
  tab = as.character(tab)
  tab = paste0(tab, collapse = "\n")
  return(tab)
}


#' @name get_shortages
#' @title Get data on drug shortages
#' @description Gets data on drug shortages from the FDA Drug Shortages API.
#' @param category:str The therapeutic category of the drug.
#' @param limit:int The maximum number of results to return.
#' @return A data.frame of drug shortages.
#' @export
get_shortages = function(category = "Psychiatry", limit = 500){
  # Testing values
  # category = "Psychiatry"

  # Create request object
  req = request("https://api.fda.gov/drug/shortages.json") |>
      req_headers(Accept = "application/json")  |>
      req_method("GET") |>
      # Sort by initial posting date, most recent first
      req_url_query(sort="initial_posting_date:desc") |>
      # Search for capsule medications, Psychiatric medications, and current shortages
      req_url_query(search = paste0('dosage_form:"Capsule"+status:"Current"+therapeutic_category:"', category, '"')) |>
      # Limit to N results
      req_url_query(limit = limit) 


    # Perform the request
    resp = req |> req_perform()
    # Parse the response as JSON
    data = resp_body_json(resp)

    # Process the data into a tidy dataframe
    processed_data = data |> 
      with(results) |> 
      map_dfr(~tibble(
        generic_name = .x$generic_name,
        update_type = .x$update_type,
        update_date = .x$update_date,
        availability = .x$availability,
        related_info = .x$related_info,
          )
      )  %>%
      mutate(update_date = lubridate::mdy(update_date))
      return(processed_data)
}
