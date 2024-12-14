
simulate_blueprints <- function(use_owned         = FALSE, # Can take both logical (import from xlsx) and numeric (to replicate n owned)
                                hoard             = FALSE,
                                starting_filament = 0,
                                recycle_yield     = 0.5,   # Proportion of filaments received during recycling
                                n_week            = 6,     # How many blueprints drops per week
                                time_week         = 9.5,   # Time needed to claim all weekly drops, in hours
                                print             = FALSE, # Prints a short summary for a finished simulation
                                detailed          = FALSE) # Saves information for each attempt in separate tibble
{
  
  if (!exists("blueprints") | !exists("blueprint_data")) {
    cli_abort(message = "Required data files have not been imported or are missing")
  }
  
  # Each item gets assigned its individual probability based on its tier
  items_probs <- blueprint_data$prob_item[match(blueprints$tier, blueprint_data$tier)]
  items_total <- sum(blueprint_data$n_items)
  
  # Inventory of blueprints
  collected_items <- numeric(items_total)
  
  # If to use existing owned blueprints from Excel sheet
  if (is.logical(use_owned)) {
    if (use_owned) {
      collected_items <- ifelse(blueprints$owned == TRUE, 1, 0)
    }
    # Else use n blueprints which are randomly sampled
  } else if (is.numeric(use_owned) & length(use_owned) == 1) {
    sampled_items <- sample(seq_len(items_total), 
                            size = round(use_owned, digits = 0),
                            replace = FALSE,
                            prob = items_probs)
    
    if (!is_empty(sampled_items)) {
      collected_items[sampled_items] <- collected_items[sampled_items] + 1
    } # else do nothing
    
  } else {
    cli_abort(message = c(
      "{.var use_owned} must be either logical or numeric vector with a length of 1",
      "x" = c("You've supplied an object of class {.cls {class(use_owned)}} With an length of {.cls {length(use_owned)}}")
    ))
  }
  
  filaments <- if (starting_filament > 0) starting_filament else 0
  attempts  <- 0
  recycled_items <- 0
  detailed_items <- tibble()
  
  while (TRUE) {
    
    # Stop if all blueprints have been claimed
    if (all(collected_items >= 1)) break
    
    missing_items <- which(collected_items == 0)
    total_missing_cost <- sum(sapply(missing_items, get_item_cost))
    
    # Stop if reached filament tipping point
    if (hoard & filaments >= total_missing_cost) break
    
    # Draw a random blueprint based on its probability
    new_item <- sample(blueprints$id, size = 1, prob = items_probs)
    attempts <- attempts + 1
    recycled <- FALSE
    
    # Update current inventory
    collected_items[new_item] <- collected_items[new_item] + 1
    missing_items <- which(collected_items == 0)
    total_missing_cost <- sum(sapply(missing_items, get_item_cost))
    
    # Recycle blueprint if it has already been collected
    if (collected_items[new_item] > 1) {
      filaments <- filaments + get_item_cost(new_item) * recycle_yield
      collected_items[new_item] <- collected_items[new_item] - 1
      recycled_items <- recycled_items + 1
      recycled <- TRUE
    }
    
    # Records information on rolled blueprint attempt
    if (detailed) {
      detailed_items <- bind_rows(detailed_items, 
                               tibble(
                                 attempt = attempts,
                                 received_id = new_item,
                                 received_cat = get_item_cat(new_item),
                                 received_name = get_item_name(new_item),
                                 received_tier = get_item_tier(new_item),
                                 duplicate = recycled,
                                 bought = FALSE,
                                 filament_saved = filaments, # filaments before buying
                                 filament_missing = total_missing_cost
                               ))
    }
    
    # If spending filaments and have enough to buy an blueprint regardless of tier
    # Will buy more than one blueprint if remaining filament is high enough.
    while (!hoard & can_buy_item() & any(collected_items == 0)) {
      
      # If more than one missing then randomly select one with equal probabilities
      if (length(missing_items) > 1) {
        selected_item <- sample(missing_items, 1)
        total_missing_cost <- sum(sapply(which(collected_items == 0), get_item_cost))
        
        # if there is only one missing left then buy it
      } else if (length(missing_items) == 1) {
        selected_item <- missing_items
      }
      
      collected_items[selected_item] <- collected_items[selected_item] + 1
      filaments <- filaments - get_item_cost(selected_item)
      
      # Records information on bought blueprints
      if (detailed) {
        detailed_items <- bind_rows(detailed_items,
                                 tibble(
                                   attempt = attempts,
                                   received_id = selected_item,
                                   received_cat = get_item_cat(selected_item),
                                   received_name = get_item_name(selected_item),
                                   received_tier = get_item_tier(selected_item),
                                   duplicate = FALSE,
                                   bought = TRUE,
                                   filament_saved = filaments, # filaments after buying
                                   filament_missing = total_missing_cost
                                 ))
      }
    }
  }
  
  weeks <- round(attempts / n_week, digits = 1)
  years <- round(weeks / 52, digits = 1)
  hours <- round(weeks * time_week, digits = 1)
  
  if (print) {
    cat("Total attempts:", attempts, "\n")
    cat("Total hours:", hours, "\n")
    cat("Total weeks:", weeks, "\n")
    cat("Total years:", years, "\n")
    cat("Number of blueprints recycled:", recycled_items, "\n")
    if (hoard) cat("Filament tipping point:", filaments, "\n")
  }
  
  results <- tibble(
    total_attempts = attempts,
    hours = hours,
    weeks = weeks,
    years = years,
    recycled = recycled_items
  )
  
  results$filament_tip <- if (hoard) filaments else NA
  
  if (detailed) {
    results <- list(results = results, detailed = detailed_items)
  }
  
  return(invisible(results))
}

get_item <- function(id) {
  get_item_info(id, mode = "all")
}

get_item_cat <- function(id) {
  get_item_info(id, mode = "cat")
}

get_item_name <- function(id) {
  get_item_info(id, mode = "name")
}

get_item_tier <- function(id) {
  get_item_info(id, mode = "tier")
}

get_item_cost <- function(id) {
  get_item_info(id, mode = "cost")
}

get_item_info <- function(id, mode) {
  
  item <-  blueprints[blueprints$id == id,]
  
  switch(mode,
         all  = item,
         cat  = item$secondary,
         name = item$item_name,
         tier = item$tier,
         cost = blueprint_data[blueprint_data$tier == item$tier,]$cost)
}

# Checks if filament is enough for certain intervals of total missing cost
# Fixes issues when the missing cost is less than the cost of the max tier
can_buy_item <- function() {
  
  data <- get("blueprint_data")
  filaments <- get("filaments")
  missing_cost <- get("total_missing_cost")
  
  missing_tier <- data |>
    arrange(desc(cost)) |>
    filter(cost <= missing_cost) |>
    pull(tier) |>
    first()
  
  max_cost <- data |>
    filter(tier == missing_tier) |>
    pull(cost)
  
  filaments >= max_cost
}
