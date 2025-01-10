.create_message <-
  function(n, ft_data, role_subjet, role) {
    list(
      "messages" = data.frame(
        role = role,
        content = c(
          role_subjet,
          ft_data$question[n],
          ft_data$true_answer[n]
        )
      )
    )
  }
