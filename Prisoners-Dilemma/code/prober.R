#This strategy probes the opponent by responding to their defections with a series of defections, aiming to test how the opponent responds to negative behavior.


prober <- function(opponent_actions, my_actions) {
    # Number of rounds to defect after an opponent defect
    rounds_to_defect <- 3
    defecting_rounds <- 0  # Counter for defecting rounds

    round_count <- length(my_actions)

    if (round_count == 0) {
        return("Cooperate")  # In the first round, always cooperate
    }

    # Check if the opponent defected in the last round
    if (length(opponent_actions) > 0 && tail(opponent_actions, 1) == "Defect") {
        defecting_rounds <- 1  # Start defecting
    }

    # If defecting, continue to defect for the specified rounds
    if (defecting_rounds > 0) {
        if (defecting_rounds < rounds_to_defect) {
            defecting_rounds <- defecting_rounds + 1  # Increment the defecting rounds
            return("Defect")
        } else {
            defecting_rounds <- 0  # Reset the defecting rounds
        }
    }

    return("Cooperate")  # Default to cooperating
}
