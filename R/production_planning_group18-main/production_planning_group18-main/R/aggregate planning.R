# =====================================================================
#  AGGREGATE PLANNING MODULE  (Assignment 3)
#  File: R/aggregate_planning.R
#
#  Part 1 - calculation functions (plain R, no Shiny)
#  Part 2 - UI function      : aggregate_planning_ui(id)
#  Part 3 - server function  : aggregate_planning_server(id)
# =====================================================================

BIGM <- 1e7    # "forbidden cell" cost in the transportation table
EPS  <- 1e-6   # numerical tolerance

# ---------------------------------------------------------------------
# PART 1A - small helpers
# ---------------------------------------------------------------------

# Pretty number: 12.5 -> "12.5", 1234 -> "1,234", NA -> ""
fmt_num <- function(x, digits = 2) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    v <- round(v, digits)
    if (abs(v) < 10^(-digits)) v <- 0
    format(v, big.mark = ",", trim = TRUE, scientific = FALSE)
  }, character(1))
}

is_fractional <- function(W) any(abs(W - round(W)) > EPS)

# Net demand per period = demand adjusted for starting / ending inventory.
# Starting inventory is used up first; ending inventory is added to the last period.
net_demand <- function(D, s0, e) {
  net <- diff(c(0, pmax(0, cumsum(D) - s0)))
  net[length(net)] <- net[length(net)] + e
  net
}

# Example data (used until the user uploads a file)
example_data <- data.frame(
  Period  = month.abb,
  Demand  = c(1800, 1500, 1100, 900, 1100, 1600, 2200, 2400, 2600, 2000, 1500, 1700),
  Days    = c(22, 20, 21, 22, 21, 22, 23, 21, 22, 22, 21, 22),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------
# PART 1B - reading the uploaded file
# ---------------------------------------------------------------------
clean_planning_data <- function(df) {
  if (ncol(df) < 3) stop("The file needs 3 columns: Period, Demand, Working days.")
  nm <- tolower(names(df))
  pick <- function(pattern, default) {
    i <- grep(pattern, nm)
    if (length(i) > 0) i[1] else default
  }
  ip <- pick("period|month|name", 1)
  id <- pick("demand|forecast", 2)
  iw <- pick("day", 3)
  out <- data.frame(
    Period = as.character(df[[ip]]),
    Demand = suppressWarnings(as.numeric(df[[id]])),
    Days   = suppressWarnings(as.numeric(df[[iw]])),
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$Demand) & !is.na(out$Days), , drop = FALSE]
  if (nrow(out) == 0) stop("No valid rows found. Demand and working days must be numbers.")
  out
}

read_planning_file <- function(path, name) {
  ext <- tolower(tools::file_ext(name))
  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(path))
  } else if (ext == "csv") {
    first <- readLines(path, n = 1, warn = FALSE)
    sep <- if (grepl(";", first, fixed = TRUE) && !grepl(",", first, fixed = TRUE)) ";" else ","
    df <- read.csv(path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Please upload an .xlsx, .xls or .csv file.")
  }
  clean_planning_data(df)
}

# ---------------------------------------------------------------------
# PART 1C - the plan data frame, costs, rounding
# ---------------------------------------------------------------------

# Builds the full plan table from a workforce vector (and optional
# overtime O, idle time U, subcontracting S in units).
#   production = K * days * workers + overtime - idle
#   inventory  = start inv + cumulative (production + subcontract) - cumulative demand
simulate_plan <- function(p, W, O = rep(0, p$n), U = rep(0, p$n), S = rep(0, p$n)) {
  change <- diff(c(p$W0, W))
  prod   <- p$K * p$days * W + O - U
  inv    <- p$s0 + cumsum(prod + S) - cumsum(p$D)
  plan <- data.frame(
    Period = p$names, Days = p$days, Demand = p$D, NetDemand = p$netd,
    Workers = W, Hired = pmax(change, 0), Fired = pmax(-change, 0),
    Production = prod, Overtime = O, Idle = U, Subcontract = S,
    Inventory = inv, stringsAsFactors = FALSE
  )
  num <- vapply(plan, is.numeric, logical(1))
  plan[num] <- lapply(plan[num], function(x) { x <- round(x, 8); x[abs(x) < 1e-8] <- 0; x })
  plan
}

cost_vector <- function(p, plan) {
  c("Hiring"         = p$hire * sum(plan$Hired),
    "Firing"         = p$fire * sum(plan$Fired),
    "Holding"        = p$hold * sum(plan$Inventory),
    "Regular time"   = p$regc * p$K * sum(plan$Workers * plan$Days),
    "Overtime"       = p$otc  * sum(plan$Overtime),
    "Subcontracting" = p$subc * sum(plan$Subcontract))
}

cost_table <- function(p, res) {
  c1 <- cost_vector(p, res$orig)
  df <- data.frame(Item = c(names(c1), "TOTAL COST"),
                   a = c(c1, sum(c1)), stringsAsFactors = FALSE)
  names(df)[2] <- "Original plan"
  if (!is.null(res$round)) {
    c2 <- cost_vector(p, res$round)
    df[["Whole-workers plan"]] <- c(c2, sum(c2))
  }
  df
}

# Table shown to the user (adds a Total row, nicer names, nicer numbers)
format_plan <- function(plan, extra = FALSE) {
  cols <- c("Period", "Days", "Demand", "NetDemand", "Workers", "Hired", "Fired", "Production")
  if (extra) cols <- c(cols, "Overtime", "Idle", "Subcontract")
  cols <- c(cols, "Inventory")
  df <- plan[, cols, drop = FALSE]
  sum_cols <- c("Days", "Demand", "NetDemand", "Hired", "Fired", "Production",
                "Overtime", "Idle", "Subcontract")
  tot <- df[1, , drop = FALSE]
  for (nm in names(df)) {
    tot[[nm]] <- if (nm == "Period") "Total" else if (nm %in% sum_cols) sum(df[[nm]]) else NA_real_
  }
  df <- rbind(df, tot)
  num <- vapply(df, is.numeric, logical(1))
  df[num] <- lapply(df[num], fmt_num)
  names(df)[names(df) == "NetDemand"]   <- "Net demand"
  names(df)[names(df) == "Idle"]        <- "Idle (units)"
  names(df)[names(df) == "Subcontract"] <- "Subcontract"
  df
}

# Cumulative production (incl. subcontracting) vs cumulative net demand
plot_cumulative <- function(p, res, title) {
  n  <- p$n
  cn <- cumsum(p$netd)
  cp <- cumsum(res$orig$Production + res$orig$Subcontract)
  cr <- if (!is.null(res$round)) cumsum(res$round$Production + res$round$Subcontract) else NULL
  plot(1:n, cn, type = "o", pch = 16, lwd = 2, col = "black", xaxt = "n",
       ylim = range(0, cn, cp, cr), xlab = "Period", ylab = "Cumulative units", main = title)
  axis(1, at = 1:n, labels = p$names, las = if (n > 8) 2 else 1)
  lines(1:n, cp, type = "o", pch = 17, lwd = 2, col = "#1f77b4")
  leg <- c("Cumulative net demand", "Cumulative production (original)")
  col <- c("black", "#1f77b4"); lty <- c(1, 1); pch <- c(16, 17)
  if (!is.null(cr)) {
    lines(1:n, cr, type = "o", pch = 15, lwd = 2, lty = 2, col = "#d62728")
    leg <- c(leg, "Cumulative production (whole workers)")
    col <- c(col, "#d62728"); lty <- c(lty, 2); pch <- c(pch, 15)
  }
  legend("topleft", legend = leg, col = col, lty = lty, pch = pch, bty = "n")
  grid()
}

# ---------------------------------------------------------------------
# PART 1D - heuristic plans
# ---------------------------------------------------------------------

# Chase (zero-inventory): produce exactly the net demand every period
plan_chase <- function(p) {
  simulate_plan(p, p$netd / (p$K * p$days))
}

# Level (constant workforce): smallest constant workforce that never runs short
#   W = max over t of ( cumulative net demand_t / (K * cumulative days_t) )
plan_level <- function(p) {
  W <- max(0, cumsum(p$netd) / (p$K * cumsum(p$days)))
  simulate_plan(p, rep(W, p$n))
}

# Mixed: workforce is constant inside each block; it may change only at the
# start of the chosen periods (period 1 always starts a block).
plan_mixed <- function(p, change_idx) {
  starts <- sort(unique(c(1, change_idx)))
  ends   <- c(starts[-1] - 1, p$n)
  cumn   <- cumsum(p$netd)
  W <- numeric(p$n)
  cum_prod <- 0
  for (k in seq_along(starts)) {
    idx <- starts[k]:ends[k]
    need <- cumn[idx] - cum_prod
    w <- max(0, need / (p$K * cumsum(p$days[idx])))
    W[idx] <- w
    cum_prod <- cum_prod + w * p$K * sum(p$days[idx])
  }
  simulate_plan(p, W)
}

# Adds the "whole workers" version (workers rounded UP) when needed
make_heuristic_result <- function(p, plan) {
  res <- list(orig = plan, round = NULL)
  if (is_fractional(plan$Workers)) {
    res$round <- simulate_plan(p, ceiling(plan$Workers - EPS))
  }
  res
}

# ---------------------------------------------------------------------
# PART 1E - linear programming
#  Variables for each period t:  W H F I O U S   (7 * n variables)
#   W workers, H hired, F fired, I ending inventory,
#   O overtime units, U idle units, S subcontracted units
#  Constraints:
#   W_t = W_(t-1) + H_t - F_t
#   I_t = I_(t-1) + (K*days_t*W_t + O_t - U_t) + S_t - D_t
#   O_t <= overtime% * K*days_t*W_t ;  U_t <= K*days_t*W_t ;  S_t <= limit
#   I_n >= ending inventory
# ---------------------------------------------------------------------
solve_lp_plan <- function(p, allow_ot, allow_idle, allow_sub, fixedW = NULL) {
  n <- p$n; nv <- 7 * n
  iW <- function(t) t;         iH <- function(t) n + t;     iF <- function(t) 2 * n + t
  iI <- function(t) 3 * n + t; iO <- function(t) 4 * n + t; iU <- function(t) 5 * n + t
  iS <- function(t) 6 * n + t
  
  rows <- list(); dirs <- character(); rhs <- numeric()
  add <- function(idx, val, dir, r) {
    row <- numeric(nv); row[idx] <- val
    rows[[length(rows) + 1]] <<- row
    dirs <<- c(dirs, dir); rhs <<- c(rhs, r)
  }
  
  for (t in 1:n) {
    kd <- p$K * p$days[t]
    if (t == 1) {
      add(c(iW(1), iH(1), iF(1)), c(1, -1, 1), "=", p$W0)
      add(c(iI(1), iW(1), iO(1), iU(1), iS(1)), c(1, -kd, -1, 1, -1), "=", p$s0 - p$D[1])
    } else {
      add(c(iW(t), iW(t - 1), iH(t), iF(t)), c(1, -1, -1, 1), "=", 0)
      add(c(iI(t), iI(t - 1), iW(t), iO(t), iU(t), iS(t)), c(1, -1, -kd, -1, 1, -1), "=", -p$D[t])
    }
    add(c(iO(t), iW(t)), c(1, -p$ot_pct / 100 * kd), "<=", 0)
    add(c(iU(t), iW(t)), c(1, -kd), "<=", 0)
    add(iS(t), 1, "<=", p$sub_cap)
    if (!allow_ot)   add(iO(t), 1, "=", 0)
    if (!allow_idle) add(iU(t), 1, "=", 0)
    if (!allow_sub)  add(iS(t), 1, "=", 0)
    if (!is.null(fixedW)) add(iW(t), 1, "=", fixedW[t])
  }
  add(iI(n), 1, ">=", p$e)
  
  obj <- numeric(nv)
  for (t in 1:n) {
    obj[iH(t)] <- p$hire
    obj[iF(t)] <- p$fire
    obj[iI(t)] <- p$hold
    obj[iW(t)] <- p$regc * p$K * p$days[t]
    obj[iO(t)] <- p$otc
    obj[iS(t)] <- p$subc
  }
  
  sol <- lpSolve::lp("min", obj, do.call(rbind, rows), dirs, rhs)
  if (sol$status != 0) return(NULL)
  x <- sol$solution
  simulate_plan(p, W = x[iW(1:n)], O = x[iO(1:n)], U = x[iU(1:n)], S = x[iS(1:n)])
}

# ---------------------------------------------------------------------
# PART 1F - transportation method
#  Sources  : Regular, Overtime, Subcontract of every period (3n rows)
#  Sinks    : net demand of every period (+ a dummy column for unused capacity)
#  Unit cost: production cost + holding cost * (periods the unit is stored)
#             Producing for an EARLIER period (backorder) is forbidden (X).
# ---------------------------------------------------------------------
alloc_nw <- function(supply, demand) {          # north-west corner rule
  m <- length(supply); k <- length(demand)
  A <- matrix(0, m, k); s <- supply; d <- demand; i <- 1; j <- 1
  while (i <= m && j <= k) {
    q <- min(s[i], d[j])
    A[i, j] <- q; s[i] <- s[i] - q; d[j] <- d[j] - q
    if (s[i] <= EPS) i <- i + 1 else j <- j + 1
  }
  A
}

alloc_least_cost <- function(supply, demand, cost) {   # cheapest cell first
  m <- length(supply); n <- length(demand)
  A <- matrix(0, m, n + 1); s <- supply; d <- demand
  while (sum(d) > EPS) {
    cm <- cost
    cm[s <= EPS, ] <- Inf
    cm[, d <= EPS] <- Inf
    if (all(is.infinite(cm)) || min(cm) >= BIGM) return(NULL)
    pos <- which(cm == min(cm), arr.ind = TRUE)[1, ]
    q <- min(s[pos[1]], d[pos[2]])
    A[pos[1], pos[2]] <- q
    s[pos[1]] <- s[pos[1]] - q
    d[pos[2]] <- d[pos[2]] - q
  }
  A[, n + 1] <- s            # everything left over goes to the dummy column
  A
}

alloc_optimal <- function(supply, demand_full, cost_full) {   # true optimum (LP check)
  # (written as a general LP: lpSolve::lp.transport can hang on fractional data)
  m <- nrow(cost_full); k <- ncol(cost_full); nv <- m * k
  A <- matrix(0, m + k, nv)
  for (i in 1:m) A[i, ((i - 1) * k + 1):(i * k)] <- 1      # each source ships at most its capacity
  for (j in 1:k) A[m + j, seq(j, nv, by = k)] <- 1         # each destination gets exactly its demand
  sol <- lpSolve::lp("min", as.vector(t(cost_full)), A,
                     c(rep("<=", m), rep("=", k)), c(supply, demand_full))
  if (sol$status != 0) return(NULL)
  matrix(sol$solution, m, k, byrow = TRUE)
}

solve_transport <- function(p, W, method) {
  n <- p$n
  cap_reg <- W * p$K * p$days
  cap_ot  <- p$ot_pct / 100 * cap_reg
  cap_sub <- rep(p$sub_cap, n)
  supply  <- as.vector(rbind(cap_reg, cap_ot, cap_sub))
  type    <- rep(c("Regular", "Overtime", "Subcontract"), times = n)
  per     <- rep(seq_len(n), each = 3)
  unit    <- unname(c(Regular = p$regc, Overtime = p$otc, Subcontract = p$subc)[type])
  m <- 3 * n
  
  cost <- matrix(BIGM, m, n)
  for (i in seq_len(m)) for (j in seq_len(n)) {
    if (j >= per[i]) cost[i, j] <- unit[i] + p$hold * (j - per[i])
  }
  demand  <- p$netd
  surplus <- sum(supply) - sum(demand)
  if (surplus < -EPS) {
    return(list(error = sprintf(
      "Total capacity (%s units) is smaller than total net demand (%s units). Increase the workforce, the overtime limit or the subcontracting limit.",
      fmt_num(sum(supply), 0), fmt_num(sum(demand), 0))))
  }
  surplus <- max(0, surplus)
  # unused regular capacity is still paid for (cost = regular cost); unused OT/sub is free
  dummy <- ifelse(type == "Regular", p$regc, 0)
  cost_full <- cbind(cost, dummy)
  
  A <- switch(method,
              nw    = alloc_nw(supply, c(demand, surplus)),
              least = alloc_least_cost(supply, demand, cost),
              opt   = alloc_optimal(supply, c(demand, surplus), cost_full))
  if (is.null(A)) return(list(error = "No feasible transportation plan was found with these inputs."))
  if (any(A[, 1:n][cost >= BIGM] > EPS)) {
    return(list(error = paste(
      "This starting solution is not feasible: it would produce for an earlier period (backorder).",
      "Try 'Cheapest cell first' or 'Optimal', or increase the capacity.")))
  }
  
  r <- seq(1, m, by = 3)
  reg_used <- rowSums(A[r, 1:n, drop = FALSE])
  idle     <- A[r, n + 1]
  ot       <- rowSums(A[r + 1, 1:n, drop = FALSE])
  sub      <- rowSums(A[r + 2, 1:n, drop = FALSE])
  plan <- simulate_plan(p, W = rep(W, n), O = ot, U = idle, S = sub)
  
  list(error = NULL, plan = plan, cost = cost_full, alloc = A, supply = supply,
       labels = paste(type, rep(p$names, each = 3)))
}

# ---------------------------------------------------------------------
# PART 1G - disaggregation (square-root rule)
#  >>> If your lecture notes use a different formula, change ONLY this
#  >>> function. It returns each family's share of the aggregate production.
# ---------------------------------------------------------------------
sqrt_rule_shares <- function(fam) {
  eoq <- sqrt(2 * fam$Setup * fam$Demand / fam$Holding)   # EOQ_i = sqrt(2 K_i lambda_i / h_i)
  eoq / sum(eoq)
}

parse_families <- function(txt) {
  df <- tryCatch(read.csv(text = txt, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE),
                 error = function(e) NULL)
  if (is.null(df) || ncol(df) < 4 || nrow(df) < 1) return(NULL)
  df <- df[, 1:4]
  names(df) <- c("Family", "Demand", "Setup", "Holding")
  for (k in 2:4) df[[k]] <- suppressWarnings(as.numeric(df[[k]]))
  if (any(is.na(df[, 2:4])) || any(df[, 2:4] <= 0)) return(NULL)
  df
}

# Splits one period's total into integers that keep the total (largest remainder)
split_integer <- function(total, shares) {
  total <- round(total)
  raw <- total * shares
  out <- floor(raw)
  left <- total - sum(out)
  if (left > 0) {
    top <- order(raw - out, decreasing = TRUE)[seq_len(left)]
    out[top] <- out[top] + 1
  }
  out
}

# =====================================================================
# PART 2 - USER INTERFACE
# =====================================================================

result_ui <- function(ns, prefix) {
  tagList(
    h4("Plan"),
    div(style = "overflow-x:auto;", tableOutput(ns(paste0(prefix, "_tbl")))),
    uiOutput(ns(paste0(prefix, "_round_ui"))),
    h4("Cumulative production vs. cumulative net demand"),
    plotOutput(ns(paste0(prefix, "_plot")), height = "360px"),
    h4("Costs"),
    tableOutput(ns(paste0(prefix, "_cost")))
  )
}

aggregate_planning_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("1. Data"),
      fileInput(ns("file"), "Upload Excel or CSV (columns: Period, Demand, Working days)",
                accept = c(".csv", ".xlsx", ".xls")),
      actionButton(ns("use_example"), "Use example data"),
      br(), br(),
      textAreaInput(ns("period_names"), "Period names (comma separated, edit freely)",
                    value = paste(example_data$Period, collapse = ", "), rows = 2),
      uiOutput(ns("names_msg")),
      hr(),
      h4("2. Costs"),
      numericInput(ns("hire"), "Hiring cost (per worker hired)", 300, min = 0),
      numericInput(ns("fire"), "Firing cost (per worker fired)", 500, min = 0),
      numericInput(ns("hold"), "Holding cost (per unit per period)", 2, min = 0),
      numericInput(ns("regc"), "Regular-time cost (per unit of regular capacity)", 8, min = 0),
      numericInput(ns("otc"), "Overtime cost (per unit)", 12, min = 0),
      numericInput(ns("subc"), "Subcontracting cost (per unit)", 14, min = 0),
      hr(),
      h4("3. Plant data"),
      numericInput(ns("W0"), "Starting workforce", 15, min = 0),
      numericInput(ns("s0"), "Starting inventory (units)", 200, min = 0),
      numericInput(ns("e"), "Ending inventory (units)", 300, min = 0),
      numericInput(ns("ot_pct"), "Overtime limit (% of regular capacity)", 20, min = 0),
      numericInput(ns("sub_cap"), "Subcontracting limit (units per period)", 300, min = 0),
      hr(),
      h4("4. Productivity (K)"),
      radioButtons(ns("k_mode"), NULL,
                   choices = c("Enter K directly" = "direct", "Compute K from past data" = "compute")),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'direct'", ns("k_mode")),
        numericInput(ns("K"), "K = units per worker per day", 5, min = 0)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'compute'", ns("k_mode")),
        numericInput(ns("k_units"), "Past units produced", 5000, min = 0),
        numericInput(ns("k_workers"), "Past number of workers", 10, min = 0),
        numericInput(ns("k_days"), "Past working days", 100, min = 0)
      )
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Data",
                 br(), uiOutput(ns("k_info")), br(),
                 tableOutput(ns("data_tbl"))
        ),
        tabPanel("Heuristic plans",
                 br(),
                 radioButtons(ns("heur_method"), "Method",
                              choices = c("Zero-inventory (chase)" = "chase",
                                          "Constant workforce (level)" = "level",
                                          "Mixed (you choose the change periods)" = "mixed"),
                              inline = TRUE),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'mixed'", ns("heur_method")),
                   uiOutput(ns("mixed_ui"))
                 ),
                 result_ui(ns, "heur")
        ),
        tabPanel("Linear programming",
                 br(),
                 checkboxInput(ns("lp_ot"), "Allow overtime", TRUE),
                 checkboxInput(ns("lp_idle"), "Allow idle time", TRUE),
                 checkboxInput(ns("lp_sub"), "Allow subcontracting", TRUE),
                 result_ui(ns, "lp")
        ),
        tabPanel("Transportation method",
                 br(),
                 radioButtons(ns("tr_method"), "Solution",
                              choices = c("North-west corner (starting solution)" = "nw",
                                          "Cheapest cell first (least-cost)" = "least",
                                          "Optimal (checked with LP)" = "opt"),
                              selected = "least"),
                 radioButtons(ns("tr_wf"), "Constant workforce used for regular time",
                              choices = c("Use the starting workforce" = "start", "Enter manually" = "manual"),
                              inline = TRUE),
                 conditionalPanel(
                   condition = sprintf("input['%s'] == 'manual'", ns("tr_wf")),
                   numericInput(ns("tr_W"), "Workforce", 15, min = 0)
                 ),
                 result_ui(ns, "tr"),
                 h4("Unit cost table (X = not allowed)"),
                 div(style = "overflow-x:auto;", tableOutput(ns("tr_cost_matrix"))),
                 h4("Allocation table (units)"),
                 div(style = "overflow-x:auto;", tableOutput(ns("tr_alloc")))
        ),
        tabPanel("Disaggregation",
                 br(),
                 selectInput(ns("dis_source"), "Aggregate plan to split",
                             choices = c("Zero-inventory (chase)" = "chase",
                                         "Constant workforce (level)" = "level",
                                         "Mixed" = "mixed",
                                         "Linear programming" = "lp",
                                         "Transportation method" = "tr")),
                 textAreaInput(ns("dis_fam"),
                               "Product families (CSV: Family, Demand, Setup, Holding)",
                               value = paste("Family,Demand,Setup,Holding",
                                             "Family A,9000,200,2",
                                             "Family B,6000,150,1.5",
                                             "Family C,5400,300,3", sep = "\n"),
                               rows = 5, width = "100%"),
                 helpText("Square-root rule: EOQ_i = sqrt(2 * Setup_i * Demand_i / Holding_i). ",
                          "Each family's share of production is EOQ_i / sum of all EOQs."),
                 h4("Family summary"),
                 tableOutput(ns("dis_summary")),
                 h4("Production by family and period (units)"),
                 div(style = "overflow-x:auto;", tableOutput(ns("dis_tbl"))),
                 plotOutput(ns("dis_plot"), height = "340px")
        )
      )
    )
  )
}

# =====================================================================
# PART 3 - SERVER
# =====================================================================
aggregate_planning_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---------------- data -------------------------------------------
    uploaded <- reactiveVal(NULL)
    
    observeEvent(input$file, {
      tryCatch({
        d <- read_planning_file(input$file$datapath, input$file$name)
        uploaded(d)
        updateTextAreaInput(session, "period_names", value = paste(d$Period, collapse = ", "))
      }, error = function(e) {
        showNotification(paste("Could not read the file:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    })
    
    observeEvent(input$use_example, {
      uploaded(NULL)
      updateTextAreaInput(session, "period_names", value = paste(example_data$Period, collapse = ", "))
    })
    
    data_in <- reactive({
      d <- uploaded()
      if (is.null(d)) example_data else d
    })
    
    periods <- reactive({
      d <- data_in()
      nm <- trimws(strsplit(input$period_names, "[,;\n]")[[1]])
      nm <- nm[nzchar(nm)]
      if (length(nm) == nrow(d)) nm else d$Period
    })
    
    output$names_msg <- renderUI({
      d <- data_in()
      nm <- trimws(strsplit(input$period_names, "[,;\n]")[[1]])
      nm <- nm[nzchar(nm)]
      if (length(nm) != nrow(d)) {
        tags$p(style = "color:#b00020;",
               sprintf("You typed %d names but the data has %d periods. Original names are used.",
                       length(nm), nrow(d)))
      }
    })
    
    # ---------------- all inputs in one list -------------------------
    params <- reactive({
      d <- data_in()
      vals <- list(
        "Hiring cost" = input$hire, "Firing cost" = input$fire, "Holding cost" = input$hold,
        "Regular-time cost" = input$regc, "Overtime cost" = input$otc,
        "Subcontracting cost" = input$subc, "Starting workforce" = input$W0,
        "Starting inventory" = input$s0, "Ending inventory" = input$e,
        "Overtime limit" = input$ot_pct, "Subcontracting limit" = input$sub_cap)
      bad <- names(vals)[!vapply(vals, function(v)
        is.numeric(v) && length(v) == 1 && is.finite(v) && v >= 0, logical(1))]
      validate(need(length(bad) == 0,
                    paste("Please enter non-negative numbers for:", paste(bad, collapse = ", "))))
      
      K <- if (input$k_mode == "direct") input$K else input$k_units / (input$k_workers * input$k_days)
      validate(need(is.numeric(K) && length(K) == 1 && is.finite(K) && K > 0,
                    "K must be a positive number. Check the productivity inputs."))
      validate(need(all(d$Days > 0), "Working days must be positive in every period."),
               need(all(d$Demand >= 0), "Demand cannot be negative."))
      
      list(n = nrow(d), D = d$Demand, days = d$Days, names = periods(),
           netd = net_demand(d$Demand, input$s0, input$e),
           hire = input$hire, fire = input$fire, hold = input$hold,
           regc = input$regc, otc = input$otc, subc = input$subc,
           W0 = input$W0, s0 = input$s0, e = input$e, K = K,
           ot_pct = input$ot_pct, sub_cap = input$sub_cap)
    })
    
    output$k_info <- renderUI({
      p <- params()
      tags$p(strong("K = "), sprintf("%.4f units per worker per day", p$K),
             " | ", strong("Periods: "), p$n,
             " | ", strong("Total demand: "), fmt_num(sum(p$D), 0),
             " | ", strong("Total net demand: "), fmt_num(sum(p$netd), 0))
    })
    
    output$data_tbl <- renderTable({
      p <- params()
      data.frame(Period = p$names, `Working days` = p$days, `Demand forecast` = fmt_num(p$D),
                 `Net demand` = fmt_num(p$netd), check.names = FALSE)
    }, striped = TRUE, bordered = TRUE)
    
    # ---------------- heuristic plans --------------------------------
    output$mixed_ui <- renderUI({
      p <- params()
      if (p$n < 2) return(NULL)
      sel <- isolate(input$mixed_change)
      if (is.null(sel)) sel <- if (p$n >= 4) as.character(seq(4, p$n, by = 3)) else character(0)
      sel <- sel[as.integer(sel) <= p$n]
      checkboxGroupInput(ns("mixed_change"),
                         "Periods in which the workforce changes (period 1 always starts from the starting workforce)",
                         choices = setNames(as.character(2:p$n), p$names[2:p$n]),
                         selected = sel, inline = TRUE)
    })
    
    res_chase <- reactive({ p <- params(); make_heuristic_result(p, plan_chase(p)) })
    res_level <- reactive({ p <- params(); make_heuristic_result(p, plan_level(p)) })
    res_mixed <- reactive({
      p <- params()
      ch <- as.integer(input$mixed_change)
      ch <- ch[!is.na(ch) & ch >= 2 & ch <= p$n]
      make_heuristic_result(p, plan_mixed(p, ch))
    })
    res_heur <- reactive({
      switch(input$heur_method, chase = res_chase(), level = res_level(), mixed = res_mixed())
    })
    heur_title <- function() {
      switch(input$heur_method, chase = "Zero-inventory (chase) plan",
             level = "Constant-workforce (level) plan", mixed = "Mixed plan")
    }
    
    # ---------------- linear programming -----------------------------
    res_lp <- reactive({
      p <- params()
      orig <- solve_lp_plan(p, input$lp_ot, input$lp_idle, input$lp_sub)
      validate(need(!is.null(orig),
                    "The LP has no feasible solution. Allow overtime / subcontracting or raise their limits."))
      res <- list(orig = orig, round = NULL)
      if (is_fractional(orig$Workers)) {
        res$round <- solve_lp_plan(p, input$lp_ot, input$lp_idle, input$lp_sub,
                                   fixedW = ceiling(orig$Workers - EPS))
      }
      res
    })
    
    # ---------------- transportation ---------------------------------
    tr_workforce <- reactive({
      W <- if (input$tr_wf == "start") input$W0 else input$tr_W
      validate(need(is.numeric(W) && length(W) == 1 && is.finite(W) && W >= 0,
                    "Enter a valid workforce."))
      W
    })
    
    res_tr <- reactive({
      p <- params(); W <- tr_workforce()
      tr <- solve_transport(p, W, input$tr_method)
      validate(need(is.null(tr$error), tr$error))
      res <- list(orig = tr$plan, round = NULL, tr = tr)
      if (is_fractional(W)) {
        tr2 <- solve_transport(p, ceiling(W - EPS), input$tr_method)
        if (is.null(tr2$error)) res$round <- tr2$plan
      }
      res
    })
    
    output$tr_cost_matrix <- renderTable({
      p <- params(); tr <- res_tr()$tr
      df <- as.data.frame(ifelse(tr$cost >= BIGM, "X", fmt_num(tr$cost)), stringsAsFactors = FALSE)
      names(df) <- c(p$names, "Unused capacity")
      cbind(Source = tr$labels, df)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    output$tr_alloc <- renderTable({
      p <- params(); tr <- res_tr()$tr
      A <- tr$alloc
      df <- as.data.frame(matrix(ifelse(A > EPS, fmt_num(A), ""), nrow(A)), stringsAsFactors = FALSE)
      names(df) <- c(p$names, "Unused capacity")
      cbind(Source = tr$labels, Capacity = fmt_num(tr$supply), df)
    }, striped = TRUE, bordered = TRUE, spacing = "s")
    
    # ---------------- shared output builder --------------------------
    register_result <- function(prefix, res, title, extra = FALSE) {
      output[[paste0(prefix, "_tbl")]] <- renderTable(
        format_plan(res()$orig, extra), striped = TRUE, bordered = TRUE, spacing = "s", align = "r")
      
      output[[paste0(prefix, "_round_ui")]] <- renderUI({
        r <- res()
        if (is.null(r$round)) {
          tags$p(em("Workers are already whole numbers in every period, so no rounding is needed."))
        } else {
          tagList(h4("Whole-workers plan (workers rounded up)"),
                  div(style = "overflow-x:auto;", tableOutput(ns(paste0(prefix, "_tbl2")))))
        }
      })
      
      output[[paste0(prefix, "_tbl2")]] <- renderTable({
        r <- res(); req(r$round)
        format_plan(r$round, extra)
      }, striped = TRUE, bordered = TRUE, spacing = "s", align = "r")
      
      output[[paste0(prefix, "_plot")]] <- renderPlot(plot_cumulative(params(), res(), title()))
      
      output[[paste0(prefix, "_cost")]] <- renderTable(
        cost_table(params(), res()), striped = TRUE, bordered = TRUE,
        digits = 2, format.args = list(big.mark = ","))
    }
    
    register_result("heur", res_heur, heur_title, extra = FALSE)
    register_result("lp",   res_lp,   function() "Linear programming plan", extra = TRUE)
    register_result("tr",   res_tr,   function() "Transportation method plan", extra = TRUE)
    
    # ---------------- disaggregation ---------------------------------
    families <- reactive({
      f <- parse_families(input$dis_fam)
      validate(need(!is.null(f),
                    "Enter families as CSV with a header: Family,Demand,Setup,Holding (all numbers positive)."))
      f
    })
    
    dis_source <- reactive({
      switch(input$dis_source, chase = res_chase()$orig, level = res_level()$orig,
             mixed = res_mixed()$orig, lp = res_lp()$orig, tr = res_tr()$orig)
    })
    
    dis_matrix <- reactive({
      plan <- dis_source(); fam <- families(); sh <- sqrt_rule_shares(fam)
      total <- plan$Production + plan$Subcontract
      M <- t(vapply(total, function(x) split_integer(x, sh), numeric(length(sh))))
      if (length(sh) == 1) M <- matrix(M, ncol = 1)
      colnames(M) <- fam$Family
      rownames(M) <- plan$Period
      M
    })
    
    output$dis_summary <- renderTable({
      fam <- families(); sh <- sqrt_rule_shares(fam); M <- dis_matrix()
      eoq <- sqrt(2 * fam$Setup * fam$Demand / fam$Holding)
      data.frame(Family = fam$Family, Demand = fmt_num(fam$Demand), `Setup cost` = fmt_num(fam$Setup),
                 `Holding cost` = fmt_num(fam$Holding), EOQ = fmt_num(eoq),
                 `Share of production` = paste0(fmt_num(100 * sh, 1), "%"),
                 `Units over horizon` = fmt_num(colSums(M), 0),
                 `Lots (units / EOQ)` = fmt_num(colSums(M) / eoq, 1),
                 check.names = FALSE)
    }, striped = TRUE, bordered = TRUE)
    
    output$dis_tbl <- renderTable({
      M <- dis_matrix()
      df <- data.frame(Period = rownames(M), apply(M, 2, fmt_num, digits = 0),
                       Total = fmt_num(rowSums(M), 0), check.names = FALSE, stringsAsFactors = FALSE)
      tot <- c("Total", fmt_num(colSums(M), 0), fmt_num(sum(M), 0))
      names(tot) <- names(df)
      rbind(df, tot)
    }, striped = TRUE, bordered = TRUE, align = "r")
    
    output$dis_plot <- renderPlot({
      M <- dis_matrix()
      barplot(t(M), names.arg = rownames(M), legend.text = colnames(M), xlab = "Period",
              ylab = "Units", main = "Production by product family",
              col = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b")[seq_len(ncol(M))],
              args.legend = list(x = "topleft", bty = "n"), las = if (nrow(M) > 8) 2 else 1)
    })
  })
}