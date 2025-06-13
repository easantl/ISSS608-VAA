# app.R
# Sailor Shift Career Explorer — 纯 tidygraph + visNetwork

library(shiny)
library(pacman)
p_load(tidygraph, visNetwork, jsonlite, dplyr, rlang, igraph)

# 1. Data Loading and Preprocessing
kg        <- fromJSON("MC1_graph.json", simplifyDataFrame = TRUE)
nodes_df  <- as_tibble(kg$nodes)
edges_df  <- as_tibble(kg$links)

# Map original node IDs to row indices
id_map   <- tibble(id = nodes_df$id, index = seq_len(nrow(nodes_df)))
edges_df <- edges_df %>%
  left_join(id_map, by = c("source" = "id")) %>% rename(from = index) %>%
  left_join(id_map, by = c("target" = "id")) %>% rename(to   = index) %>%
  filter(!is.na(from), !is.na(to))

# tidygraph object
graph_tbl <- tbl_graph(
  nodes    = nodes_df,
  edges    = edges_df,
  directed = kg$directed
)

# Find the row index of "Sailor Shift" in nodes_df
sailor_idx <- which(nodes_df$name == "Sailor Shift")

# 2. Generic Subgraph Extraction Function 
extract_subgraph <- function(graph, expr) {
  g_sub <- graph %>%
    activate(edges) %>%
    filter(!!enquo(expr))
  used  <- g_sub %>%
    activate(edges) %>%
    as_tibble() %>%
    select(from, to) %>%
    unlist() %>%
    unique()
  g_sub %>%
    activate(nodes) %>%
    mutate(.row = row_number()) %>%
    filter(.row %in% used) %>%
    select(-.row)
}

# a) Influence subgraph
get_influence <- function(types) {
  works <- graph_tbl %>%
    activate(edges) %>% as_tibble() %>%
    filter(`Edge Type` == "PerformerOf", from == sailor_idx) %>%
    pull(to) %>% unique()
  focus <- unique(c(sailor_idx, works))
  extract_subgraph(
    graph_tbl,
    (`Edge Type` %in% types) & (to %in% focus)
  )
}

# b) Collaboration subgraph
get_collab <- function(depth) {
  ig    <- as.igraph(graph_tbl)
  dists <- distances(ig, v = sailor_idx, to = V(ig), mode = "all")
  valid <- which(dists <= depth)
  extract_subgraph(
    graph_tbl,
    (`Edge Type` %in% c("PerformerOf","ComposerOf","ProducerOf","LyricistOf","CoverOf")) &
      (from %in% valid & to %in% valid)
  )
}

# c) Community subgraph）
get_community <- function(depth = 1) {

  folk_idx <- graph_tbl %>%
    activate(nodes) %>%
    mutate(.row = row_number()) %>%
    as_tibble() %>%
    filter(genre == "Oceanus Folk") %>%
    pull(.row)
  

  base_sub <- extract_subgraph(
    graph_tbl,
    (`Edge Type` == "PerformerOf") &
      (from == sailor_idx & to %in% folk_idx)
  )
  if (depth == 1) return(base_sub)
  

  creative_types <- c("ComposerOf","ProducerOf","LyricistOf")
  work_ids <- base_sub %>%
    activate(nodes) %>%
    as_tibble() %>%
    mutate(.row = row_number()) %>%
    filter(`Node Type` %in% c("Album","Song")) %>%
    pull(.row)
  
  creative_sub <- extract_subgraph(
    graph_tbl,
    (`Edge Type` %in% creative_types) & (to %in% work_ids)
  )
  

  edges_all <- bind_rows(
    base_sub   %>% activate(edges) %>% as_tibble(),
    creative_sub %>% activate(edges) %>% as_tibble()
  )
  nodes_all <- bind_rows(
    base_sub   %>% activate(nodes) %>% as_tibble(),
    creative_sub %>% activate(nodes) %>% as_tibble()
  ) %>%
    distinct(name, .keep_all = TRUE) %>%
    mutate(.row = row_number())
  
  tbl_graph(nodes = nodes_all, edges = edges_all, directed = kg$directed)
}

# ----------------------------------------------------
ui <- fluidPage(
  titlePanel("Sailor Shift Career Explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("view", "Select View:：",
                   c("Influence"   = "influence",
                     "Collaboration"   = "collab",
                     "Community"   = "community")
      ),
      conditionalPanel("input.view=='influence'",
                       checkboxGroupInput("inf_types", "Influence Type：",
                                          choices  = c("ComposerOf","ProducerOf","LyricistOf","CoverOf"),
                                          selected = c("ComposerOf","ProducerOf")
                       )
      ),
      conditionalPanel("input.view=='collab'",
                       numericInput("depth", "Depth:",
                                    value = 1, min = 1, max = 3
                       )
      ),
      conditionalPanel("input.view=='community'",
                       numericInput("com_depth", "Depth：",
                                    value = 1, min = 1, max = 2
                       )
      ),
      helpText("Click nodes for details")
    ),
    mainPanel(
      visNetworkOutput("network", height = "700px")
    )
  )
)


server <- function(input, output, session) {

  sub_tbl <- reactive({
    switch(input$view,
           "influence" = get_influence(input$inf_types),
           "collab"    = get_collab(input$depth),
           "community" = get_community(input$com_depth)
    )
  })
  

  output$network <- renderVisNetwork({
    tblg <- sub_tbl()
    vn   <- toVisNetworkData(tblg)
    
    # nodes: id, label, group
    nodes_v <- vn$nodes %>%
      rename(name = label) %>%
      mutate(
        id    = as.character(id),
        label = name,
        group = as.character(`Node Type`)
      )
    
    # edges: from, to, label, title
    edges_v <- vn$edges %>%
      mutate(
        label = as.character(`Edge Type`),
        title = paste0("Edge Type: ", label)
      )
    
    visNetwork(nodes_v, edges_v, height = "700px", width = "100%") %>%
      visEdges(arrows = "to") %>%
      visOptions(
        highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
        nodesIdSelection = TRUE,
        selectedBy       = "group"
      ) %>%
      visLegend() %>%
      visLayout(randomSeed = 2025) %>%
      visInteraction(navigationButtons = TRUE)
  })
}

# ─── 5. Run App ───────────────────────────────────────────────────────────
shinyApp(ui, server)
