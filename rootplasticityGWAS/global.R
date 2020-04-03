
library(DBI)
library(stringr)
library(dbplyr)
library(readr)
library(dplyr)
library(pool)
 # note: git push --mirror https://warrya@bitbucket.org/ADAC_UoN/maize-gwas-shiny
current_version <- "v1.3.6"

jscode <- HTML("
$(document).on('shiny:connected', function(event) {
  var imagindex = 0;
  Shiny.onInputChange('slickin', imagindex);
  $(document).on('click', '.slick-arrow', function(event) {
    var imagindex = $('.slick-active')[0].attributes[1].value;
    Shiny.onInputChange('slickin', imagindex);
  });
  $(document).on('click', '.slick-dots', function(event) {
    var imagindex = $('.slick-active')[0].attributes[1].value;
    Shiny.onInputChange('slickin', imagindex);
  });
});
")
#imgs <- list.files("./slide_show/", pattern=".jpg", full.names = TRUE)

# read/setup login related stuff
#load_passwds <- read_tsv("./password_hd5.txt", skip_empty_rows =TRUE, col_names = TRUE, col_types = cols())
#credentials  <- as.list(load_passwds$pwd_hd5)
#credentials  <- setNames(credentials, load_passwds$username)

# setup some info as vectors
chrs <- c('all' , as.character(1:10))
full_text <- c("WW"="Well Watered", 
               "WS"="Water Stressed",
               "ENVPL"="Environmental Plasticity",
               "STRPL"="Stress Plasticity",
               "unselected"="Phene Unselected",
               
               "AZ"="Arizona", 
               "SA"="South Africa",
               "AZSA"="Arizona vs South Africa",
               
               "ANGLE"="Root Angle",
               "DISTLAT"="Dist to First Lateral",
               "BF"="Lateral Branching Freq",
               "LL"="Avg Lateral Length")
# mydb <- dbConnect(RSQLite::SQLite(), "../zm_root_gwas_all_db.sqlite")
## database connection
#mydb <- dbConnect(RSQLite::SQLite(), "./data/zm_root_gwas_db.sqlite")
mydb <- dbConnect(RSQLite::SQLite(), "./data/zm_root_gwas_all_db_update_2.sqlite")
 # mydb <- dbPool(drv = RSQLite::SQLite(), dbname = "./data/zm_root_gwas_db.sqlite")
loadDataFromSQLite <- function(gwasTableName) {
  # Connect to the database
 # db <- dbConnect(SQLite(), sqlitePath)
 # mydb <- dbConnect(RSQLite::SQLite(), "./data/zm_root_gwas_db.sqlite")
  # Construct the fetching query from gwas_table_selection()
# local_gwas_data <- as.data.frame(tbl(mydb, gwasTableName))
  local_gwas_data <- as_tibble(tbl(mydb, gwasTableName))
 # query <- sprintf("SELECT * FROM %s", table)
  # Submit the fetch query and disconnect
  #data <- dbGetQuery(db, query)
#  dbDisconnect(mydb)
  local_gwas_data
}
# read data from files
biomart_orthologs_At <- read_tsv("./data/Zm_snp_targets_Arabidopsis.txt", col_types = cols())
biomart_orthologs_Os <- read_tsv("./data/Zm_snp_targets_Oryza.txt",       col_types = cols())

csv_info <- read_tsv("./data/gwas_csv_info.txt", col_types = cols())
snp_result_info_raw <- read_tsv("./data/SNP_selected_results.txt", col_types = cols()) 
conekt_links <- read_tsv("./data/conekt_link_snp_gene_matches.txt", col_types = cols()) 
ricexpro_links <- read_tsv("./data/RiceXPro_links.txt", col_types = cols()) %>% group_by(OryzaID) %>% summarise(RXP_4001 = paste(RXP_4001, collapse="</br>"))

biomart_orthologs_Os <- left_join(biomart_orthologs_Os, ricexpro_links) %>% select(-OryzaID)

snp_result_info <- snp_result_info_raw %>% 
  # convert acc to weblink
  mutate(ENSEMBL_link = paste0("<a target='_blank' href='https://plants.ensembl.org/Zea_mays/Gene/Summary?g=",
                                V4_Gene_Model, "'>ENSEMBL: ", V4_Gene_Model, '</a>')) %>% 
  mutate(MAIZEGDB_link = paste0("<a target='_blank' href='https://www.maizegdb.org/gene_center/gene/",
                                V4_Gene_Model, "'>MAIZEGDB: ", V4_Gene_Model, '</a>')) %>% 
  mutate(GRAMENE_compara_link = paste0("<a target='_blank' href='http://ensembl.gramene.org/Zea_mays/Gene/Compara_Tree?g=",
                                V4_Gene_Model, "'>Plant Compara: ", V4_Gene_Model, '</a>')) %>% 
  mutate(STRINGDB_link = paste0("<a target='_blank' href='https://string-db.org/cgi/network.pl?identifier=",
                                V3_Gene_Model, "'>STRING-db: ", V3_Gene_Model, '</a>')) %>% 
  mutate(PHYLOMEDB_link = paste0("<a target='_blank' href='http://phylomedb.org/?q=search_tree&seqid=",
                                V3_Gene_Model, "'>Phylome-db: ", V3_Gene_Model, '</a>')) %>%
#  mutate(BAREFPMAIZE_link = paste0("<a target='_blank' href='http://bar.utoronto.ca/efp_maize/cgi-bin/efpWeb.cgi?dataSource=Maize_Root&modeInput=Absolute&primaryGene=",
#                                V3_Gene_Model, "'>Maize eFP:", V3_Gene_Model, '</a>')) %>%
   mutate(PLAZA_link = paste0("<a target='_blank' href='https://bioinformatics.psb.ugent.be/plaza/versions/plaza_v4_monocots/gene_families/view_orthologs/",
                               V4_Gene_Model, "'>Plaza: ", V4_Gene_Model, '</a>'))  %>% 
  left_join(conekt_links) %>% select(-CONEKT_link)

snp_result_info_exprssn <- snp_result_info_raw %>% 
  mutate(BAREFPMAIZE_link = paste0("<a target='_blank' href='http://bar.utoronto.ca/efp_maize/cgi-bin/efpWeb.cgi?dataSource=Maize_Root&modeInput=Absolute&primaryGene=",
                                V3_Gene_Model, "'>Maize eFP: ", V3_Gene_Model, '</a>')) %>%
 mutate(EXPRESSIONATLAS_link = paste0("<a target='_blank' href='https://www.ebi.ac.uk/gxa/genes/",tolower(V4_Gene_Model),
                                "?bs=%7B%22zea%20mays%22%3A%5B%22ORGANISM_PART%22%5D%7D&ds=%7B%22kingdom%22%3A%5B%22plants%22%5D%7D#baseline'>Expression Atlas:",
                                V4_Gene_Model, '</a>')) %>% 
 mutate(QTELLER_link = paste0("<a target='_blank' href='https://qteller.maizegdb.org/bar_chart_B73v4.php?name=",
                               V4_Gene_Model, "'>qTeller: ", V4_Gene_Model, '</a>'))  %>% 
 mutate(QTELLERROOT_link = paste0("<a target='_blank' href='https://qteller.maizegdb.org/bar_chart_B73v4.php?name=",V4_Gene_Model,
"&info=CrownRoot_Node4_V7|CrownRoot_Nodes_1_3_V7|run_6DAS_GH_Primary_Root|TapRoot_Z1_7d|TapRoot_Z2_7d|TapRoot_Z3_7d|TapRoot_Z4_7d|B73_V1_4D_PE_Primary_root|WholePrimaryRoot_7d|WholeRootSystem_3d|MZ|EZ|Cortex|PR|SR'>qTeller Roots: ", 
                               V4_Gene_Model, '</a>'))  %>% 
  left_join(conekt_links)
  
#arabidopsis eFP http://bar.utoronto.ca/efp/cgi-bin/efpWeb.cgi?dataSource=Root&modeInput=Absolute&primaryGene=AT3G06080

##dbListTables(mydb)

#View(csv_info)
#View(snp_result_info)
#arbc_gwas <- csv_info$table_name[grepl("^ARBC", csv_info$table_name)]
#urbc_gwas <- csv_info$table_name[grepl("^URBC", csv_info$table_name)]
#SNPs <- read_tsv("SNP_list.txt")
