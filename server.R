library(shiny)
library(shinyjs)
library(dplyr)
library(tidyr)
library(DT)

options(shiny.maxRequestSize = 1000 * 1024^2)

server <- function(input, output, session) {
  observeEvent(input$generate, {
    output$output_text <- renderText({"Working... Please Wait."}) # Message for user patience
    delay(100, { # Delay to display message correctly
      
      msstats_data <- reactiveVal(NULL)
      ttest_data <- reactiveVal(NULL)
      
      # Process Msstats
      msstats_df <- read.table(input$msstatsFile$datapath, header = TRUE, sep = "\t")
      msstats_df$log2FC <- replace(msstats_df$log2FC, is.infinite(msstats_df$log2FC) & msstats_df$log2FC < 0, -5)
      msstats_df$log2FC <- replace(msstats_df$log2FC, is.infinite(msstats_df$log2FC) & msstats_df$log2FC > 0, 5)
      msstats_df <- msstats_df %>%
        separate(Label, into = c("Bait", "Control"), sep = "-")  %>%
        rename(Prey = Protein)
      msstats_data(msstats_df)
      
      # Process Ttest
      ttest_df <- read.table(input$ttestFile$datapath, header = TRUE, sep = "\t")
      ttest_df <- ttest_df %>%
        filter(!grepl(";", Prey)) %>%
        filter(!grepl("CON_", Prey))
      ttest_data(ttest_df)
      
      # Merge tables
      merged_df <- msstats_data() %>%
        full_join(ttest_data(), by = c("Bait", "Prey","Control"))
      
      final_results <- merged_df %>%
        mutate(
          Ttest_pvalue_col_name = paste(Bait,Control,"Ttest_pvalue", sep = "-"),
          MSstats_adj_pvalue_col_name = paste(Bait,Control,"MSstats_adj.pvalue", sep = "-"),
          log2FC_col_name = paste(Bait,Control, "log2FC", sep = "-")
        ) %>%
        pivot_wider(names_from = Ttest_pvalue_col_name, values_from = p.value) %>%
        pivot_wider(names_from = MSstats_adj_pvalue_col_name, values_from = adj.pvalue) %>%
        pivot_wider(names_from = log2FC_col_name, values_from = log2FC) %>%
        
        # Select only Prey and the newly made columns
        select(Prey,Gene, ends_with("-Ttest_pvalue"), ends_with("-MSstats_adj.pvalue"), ends_with("-log2FC")) %>%
        select(Prey,Gene, sort(setdiff(names(.), c("Prey","Protein")))) %>%
        group_by(Prey) %>%
       rename(PreyGene = Gene) %>%
        summarize(across(everything(), ~ paste(na.omit(unique(unlist(.))), collapse = ", ")), .groups = 'drop')
      
      output$mergedTable <- renderDT({final_results})
      output$output_text <- renderText({"Done."})
    })
    
    output$download_mergedTable <- downloadHandler(
      filename = function() { "TreatmentRawDataset.txt" },
      content = function(file) {
        write.table(merged_df, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
    
    output$download_final_results <- downloadHandler(
      filename = function() { "TreatmentDataset.txt" },
      content = function(file) {
        write.table(final_results, file, sep = "\t", row.names = FALSE, quote = FALSE)
      }
    )
  })
}
