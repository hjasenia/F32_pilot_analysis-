setwd("/Users/jhartman3/Documents/postdoc/ACLEW_prelim-data/random")
library(tidyr)
library(tidyverse)

#imports raw data for ACLEW and changes each column to its appropriate type 
ACLEW_all_annotations <- read.csv("ACLEW_all_annotations.csv")
ACLEW_all_annotations$recording_id <- as.character(ACLEW_all_annotations$recording_id)   #set columns as factors 
ACLEW_all_annotations$participant <- as.factor(ACLEW_all_annotations$participant) 
ACLEW_all_annotations$annotation<- as.factor(ACLEW_all_annotations$annotation) 
ACLEW_all_annotations$vcm<- as.factor(ACLEW_all_annotations$vcm) 


#first need to update C to reflect new value system 
#we are doing this to distinguish utterances intended only for other children
#old values for C combines instances where both children are addressed with ones when only other children is addressed 
#so we are adding more values to our addressee tier 
#the new value system: T (target child), X (unsure), M (both munchkins, or children), K (only non_target) 
#T is already present; We will change all Cs to K. Then the file will be saved and the other values will be added manually 

ACLEW_all_annotations$xds[ACLEW_all_annotations$xds == "C"] <- "K"
ACLEW_all_anno_updated <- ACLEW_all_annotations
write.csv(ACLEW_all_anno_update, "ACLEW_all_annotations_update.csv")

#load ACLEW file w updated addressee values 
ACLEW_all_anno_updated <- read.csv("ACLEW_all_annotations_updated.csv")

#load addressee key 
ACLEW_addresee_key <- read.csv("addressee_key.csv")

#analyzes mean utterance at a group level  
ACLEW_amount_xds_group <- ACLEW_all_anno_updated %>%                        
  filter(!participant == "CHI" & !recording_id == "3749_scrubbed.eaf" & !xds %in% c('X','M')) %>%    #exclude speech from target child and 3749_scrubbed
  group_by(recording_id) %>%
  count(xds)     %>%                                        #counts the number of utterances per addressee for each subject 
  group_by(xds) %>% 
  summarise(average = mean(n),
            N = n_distinct(recording_id), 
            SD = sd(n),
            SE = SD/sqrt(N),
            lower=average-SE,
            upper=average+SE)

ACLEW_amount_xds_group <- left_join(ACLEW_amount_xds_group, ACLEW_addresee_key)                  #adds key to summary table 


#analyzes mean utterance at an individual level 
ACLEW_amount_xds_individual <- ACLEW_all_anno_updated %>%
  filter(!participant == "CHI" & !recording_id == "3749_scrubbed.eaf" & !xds %in% c('X','M')) %>%    #exclude speech from target child and 3749_scrubbed
  group_by(recording_id) %>%
  count(xds) %>%

 ACLEW_amount_xds_individual <- left_join(ACLEW_amount_xds_individual, ACLEW_addresee_key)     #adds key to summary table


#analyses for linguistic measures: MLU, TTR, prop of utterance types (lines 57 - )
#download the required library 
library(stringr)

#cleaning dataframe  
#cleans ACLEW_all_anno_updated df by removing strings that follow a specified pattern 
#filter fxn removes entire utterance 
#str_remove_all only removes segments of string that matches the indicated pattern

ACLEW_all_anno_clean <- ACLEW_all_anno_updated %>%  
  filter(!str_detect(annotation, "xxx"), participant != "CHI", !str_detect(annotation, pattern = "(.@l|.@s|.@c)")) %>%   #removes utterances that contain codeswitching, child-talk, or spellings 
  mutate(annotation = str_remove_all(annotation, pattern = "(\\[)[=](.*)?[(\\])]"))%>%                                   #removes non-linguistic communicative noise during talks (so, ones in brackets)
  mutate(annotation = str_remove_all(annotation, pattern = "(&)([^ ]+)"))   %>%                                          #removes non-linguistic communicative noise not of brackets  (so, ones that start with & symbol)
  mutate(annotation = str_remove_all(annotation, pattern = "<(.*?)>")) %>%                                               #removes misspellings 
  mutate(annotation = str_remove_all(annotation, pattern = "(\\[:)|(])"))%>%                                             #remove correct spellings from square brackets 
  mutate(annotation = str_remove_all(annotation, pattern = "(<)|(>)|(\\.)|(,)|(!)|(\\?)")) %>%                           #removes punctuation
  mutate(annotation = str_remove_all(annotation, pattern = "(^-)|(-$)|(--)|(-\\s)"))    %>%                              #removes hyphens (indicate interrupted speech)
  filter(!str_detect(annotation, pattern = "(\\[)(spa adios)|(\\[)(fra au revoir)"))    %>%                              #remove specific lines in spanish or french that were not caught by filter for codeswitches 
  filter(!str_detect(annotation, pattern = "(dax|shang|banoona)"))                                                       #removes utterances that contain novel words (e.g. dax)

ACLEW_all_anno_clean$annotation <- str_squish(ACLEW_all_anno_clean$annotation)                                           #remove excess white space in beginning, middle, and end of string
ACLEW_all_anno_clean <- ACLEW_all_anno_clean[!ACLEW_all_anno_clean$annotation == "",]                                    #remove empty rows (these are extra spaces created when utterance were removed) 

#this script is necessary for something 

ACLEW_all_anno_clean <- ACLEW_all_anno_clean %>% 
  mutate(sentence_id = row_number())                                                                                     #creates a sentence ID for each utterance using the row #  

#some utterances have contractions. 
#for utterances w contracts, we want morphemes to be an individual token. So the contraction he's would be counted as he + 's 
#to do this, we have to replace all strings with a comma with a comma followed by space 
#then we separate each token into it's separate row 
ACLEW_all_tokens <- ACLEW_all_anno_clean %>% 
  mutate(annotation = str_replace_all(annotation, pattern = "'", replacement = " '"))                                    
ACLEW_all_tokens <- separate_rows(ACLEW_all_tokens, annotation, sep = " ")


#ACLEW_all_annotations_clean <- ACLEW_all_annotations_clean %>% 
#mutate(sentence_id = row_number())  

# ACLEW_func_content_tot<- separate_rows(ACLEW_func_content_tot, annotation, sep = " ")              #splits the utterances into words; each word is in a row   

# ACLEW_func_content_tot <- ACLEW_func_content_tot[!ACLEW_func_content_tot$annotation == "",]        #remove empty rows (these are extra spaces created when utterance were removed)

utt_length <- ACLEW_all_tokens %>% 
  group_by(sentence_id) %>% 
  summarize(token_id = row_number(),
            sentence_length = n())


ACLEW_all_tokens <- bind_cols(ACLEW_all_tokens, utt_length, .name_repair = "unique")
ACLEW_all_tokens <- select(ACLEW_all_tokens, -sentence_id...10)
ACLEW_all_tokens <- rename(ACLEW_all_tokens, sentence_id = "sentence_id...11")

#ACLEW_func_content_tot$doc_id <- "doc1"
#ACLEW_func_content_tot <- rename(ACLEW_func_content_tot,sentence_id = sentence_id)

# file_ACLEW <- left_join(ACLEW_func_content_tot, ACLEW_all_annotations_clean)
# file_ACLEW <- rename(file_ACLEW,token = annotation)
# file_ACLEW<- rename(file_ACLEW, token_id = word_location)
# conllu <- as_conllu(file_ACLEW)
# 
# ggplot() +  #plot number of utterances per addressee 
#   geom_point(data = ACLEW_amount_xds, aes(x = fct_infreq(addressee, average), y = average), stat = "identity", shape = 2, size = 3) + 
#   geom_point(data = ACLEW_amount_xds_individual, aes(x = addressee, y = n, color = addressee), stat = "identity", position_jitter(width =0.1 )) + 
#   theme_bw() +
#   scale_y_continuous(breaks = seq(from = 0, to = 400, by = 25), limits = c(0,400)) + 
#   xlab("Type of Speech") + 
#   ylab("Average Number of Utterances") 


#quantifies utterance types 
library(udpipe)
dl <- udpipe_download_model(language = "english")
str(dl)

udmodel_english <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

x_ACLEW_all_annotations <- udpipe_annotate(udmodel_english, x = ACLEW_all_annotations_clean$annotation)
x_ACLEW_all_annotations <- as.data.frame(x_ACLEW_all_annotations)
str(x_ACLEW_all_annotations)

x_ACLEW_all_annotations$doc_id <- str_remove_all(x_ACLEW_all_annotations$doc_id, pattern = "doc")


x_ACLEW_all_annotations_trimmed <- x_ACLEW_all_annotations %>% 
  select(doc_id, sentence, token_id, token, lemma, upos, xpos, feats, sentence)  %>%
  mutate(func_or_con = case_when(upos == "ADJ" | upos == "NOUN"|upos == "VERB"|upos == "ADV" | upos == "INTJ"| upos == "PROPN" ~ "content",
                                 upos == "ADP" | upos == "AUX" | upos == "CCONJ"| upos == "DET" | upos == "NUM"|upos == "PART"| upos == "PRON"| upos == "SCONJ" ~ "functor",
                                 TRUE ~"other"))

x_ACLEW_all_annotations_trimmed <- rename(x_ACLEW_all_annotations_trimmed,  annotation= sentence)
x_ACLEW_all_annotations_trimmed<- rename(x_ACLEW_all_annotations_trimmed, sentence_id = doc_id) 

x_ACLEW_all_annotations_trimmed$sentence_id <- as.integer(x_ACLEW_all_annotations_trimmed$sentence_id)
x_ACLEW_all_annotations_trimmed$token_id <- as.integer(x_ACLEW_all_annotations_trimmed$token_id)

x_ACLEW_all_annotations_trimmed <- x_ACLEW_all_annotations_trimmed %>% 
  filter(!upos == "PUNCT", !upos == "SYM" , !upos == "X", !upos == "NUM", !upos == "PROPN")

ACLEW_word_by_classes <- right_join(ACLEW_all_tokens, x_ACLEW_all_annotations_trimmed) 

ACLEW_func_content_tot_xds <- ACLEW_word_by_classes %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  select(recording_id, xds, lemma, upos, func_or_con) %>% 
  group_by(recording_id, xds) %>%
  count(func_or_con)
group_by(xds, func_or_con) %>% 
  summarise(average = mean(n))

ACLEW_func_content_upos_xds <- ACLEW_word_by_classes %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  filter(upos %in% c('VERB', 'NOUN', 'ADJ', 'DET', 'PRON', 'AUX')) %>% 
  select(recording_id, xds, lemma, upos, func_or_con) %>% 
  group_by(recording_id, xds, upos) %>%
  count(func_or_con) 
group_by(xds, upos, func_or_con) %>% 
  summarise(total = sum(n),
            sdv = sd(n))


ACLEW_func_content_upos_xds$upos <- factor(ACLEW_func_content_upos_xds$upos, levels = c("VERB", 
                                                                                        "NOUN", "ADJ","ADV", "INTJ", "DET", "PRON", "AUX", "PART", "CCONJ", "SCONJ", "ADP"))

ACLEW_func_content_upos_xds$upos <- factor(ACLEW_func_content_upos_xds$upos, levels = c("ADP", 
                                                                                        "SCONJ","CCONJ","PART","AUX","PRON","DET","INTJ","ADV","ADJ","NOUN","VERB"))

ACLEW_func_content_upos_xds$func_or_con <- as.factor(ACLEW_func_content_upos_xds$func_or_con)
ACLEW_func_content_upos_xds$func_or_con <- relevel(ACLEW_func_content_upos_xds$func_or_con, ref = "functor")

ggplot(data = ACLEW_func_content_upos_xds, aes(x = fct_reorder(xds, total), y = total, fill = upos)) + 
  geom_col(position = "fill") + 
  theme_bw() +
  scale_fill_brewer(palette = "Paired",direction = 1) + 
  scale_x_discrete(labels = c("adult", "child", "target_child")) + 
  # scale_y_continuous(breaks = seq(from = 0, to = 3500, by = 500), limits = c(0,3500))  +
  xlab("Addressee ID") + 
  ylab("Total Number of Occurrence") 
# scale_fill_discrete(labels = c('VERB', 'NOUN', 'ADJ', 'ADV', 'INTERJ', 'DET', 'PRON', 'AUX', 'PART', 'CoordConj', 'SubConj', 'AdPos')) + 
#facet_wrap(~func_or_con)


ggplot(data = ACLEW_func_content_upos_xds, aes(x = fct_reorder(upos, n), y = n,color = xds)) +  #boxplot of avg # of utterances per addressee 
  geom_boxplot() + 
  #geom_point(stat = "identity") +                                                          #plots individual subjects as data point
  theme_bw() + 
  scale_y_continuous(breaks = seq(from = 0, to = 250, by = 50), limits = c(0,250)) + 
  xlab("Speech Category") + 
  ylab("Average Number of Occurrence") 


#calculates position 
ACLEW_word_by_sentence_position <- ACLEW_word_by_classes %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  filter(upos %in% c('VERB', 'NOUN', 'ADJ', 'DET', 'PRON', 'AUX')) %>% 
  group_by(sentence_id) %>% 
  mutate(sentence_position = case_when(token_id == 1 ~ "initial",
                                       token_id == sentence_length ~ "final",
                                       TRUE ~ "medial")) %>% 
  select(recording_id, xds, lemma, upos, func_or_con, sentence_position) %>% 
  group_by(recording_id, xds, upos, sentence_position) %>%
  count(func_or_con) 
group_by(xds, upos, func_or_con) %>% 
  summarise(total = sum(n),
            sdv = sd(n))


ACLEW_word_by_sentence_position$sentence_position <- as.factor(ACLEW_word_by_sentence_position$sentence_position)
ACLEW_word_by_sentence_position$sentence_position <- relevel(ACLEW_word_by_sentence_position$sentence_position, ref = "initial")
ACLEW_word_by_sentence_position$sentence_position <- fct_relevel(ACLEW_word_by_sentence_position$sentence_position, "medial", after = 1)


ggplot(data = ACLEW_word_by_sentence_position, aes(x = fct_reorder(upos, n), y = n,color = xds)) +  #boxplot of avg # of utterances per addressee 
  geom_boxplot() + 
  #geom_point(stat = "identity") +                                                          #plots individual subjects as data point
  theme_bw() + 
  scale_y_continuous(breaks = seq(from = 0, to = 150, by = 25), limits = c(0,150)) + 
  xlab("Speech Category") + 
  ylab("Average Number of Occurrence") + 
  facet_wrap(~sentence_position)


#calculate utterance type
utt_type <- seedlings_nouns %>% 
  filter(child %in% c(11,9,26,8,36,14,4,43,44,28)) %>% 
  filter(audio_video == "audio") %>% 
  select(child, utterance_type) %>% 
  group_by(child) %>% 
  count(utterance_type)

utt_type$utterance_type <- fct_relevel(utt_type$utterance_type, c("u","r", "s", "i", "n", "q", "d"))


#stacked bar of the type of utterances children hear 
ggplot(data = utt_type, aes(x = child, y =n , fill = utterance_type)) + 
  geom_col(position = "fill") + 
  theme_bw() +
  scale_fill_brewer(palette = "PuBuGn", direction = 1, name = "Utterance Type",
                    labels = c("unclear", "reading", "singing", "imperative", "non-utterance", "question", "declarative")) + 
  xlab("Child ID") + 
  ylab("Relative Proportion of Occurrence") 

top_20_nouns <- seedlings_nouns %>% 
  filter(child %in% c(11,9,26,8,36,14,4,43,44,28)) %>% 
  filter(audio_video == "audio") %>% 
  select(object, basic_level, global_basic_level, child) %>% 
  group_by(global_basic_level) %>% 
  count(global_basic_level) 

top_20_nouns$global_basic_level <- as.factor(top_20_nouns$global_basic_level)
top_20_nouns<- top_20_nouns[order(top_20_nouns$n, decreasing = TRUE),]

top_20_nouns<- head(top_20_nouns, 20)
top_20_nouns <- rename(top_20_nouns, lemma = global_basic_level)
top_20_nouns <- left_join(top_20_nouns, ACLEW_word_by_classes)
sentence_pull <- c(unique(top_20_nouns$sentence_id))

top_20_nouns_sent_context <- ACLEW_word_by_classes[ACLEW_word_by_classes$sentence_id %in% sentence_pull, ]

#sentence position of nouns 
top_20_noun_only_sent_pos <- top_20_nouns %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(sentence_id) %>% 
  mutate(sentence_position = case_when(token_id == 1 ~ "initial",
                                       token_id == sentence_length ~ "final",
                                       TRUE ~ "medial")) %>% 
  filter(upos == "NOUN") %>% 
  select(recording_id, lemma, sentence_id, sentence_position, xds)  %>%
  group_by(recording_id, lemma, xds) %>% 
  count(sentence_position) %>% 
  group_by(recording_id, xds, sentence_position) %>% 
  summarise(total = sum(n))

top_20_noun_only_sent_pos$sentence_position <- as.factor(top_20_noun_only_sent_pos$sentence_position)
top_20_noun_only_sent_pos$sentence_position <- relevel(top_20_noun_only_sent_pos$sentence_position, ref = "initial")
top_20_noun_only_sent_pos$sentence_position <- fct_relevel(top_20_noun_only_sent_pos$sentence_position, "medial", after = 1)

ggplot(data = top_20_noun_only_sent_pos, aes(x = sentence_position, y = total, color = xds)) + 
  geom_boxplot() + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(from = 0, to = 7, by = 1), limits = c(0,7)) + 
  xlab("Sentence Position") + 
  # scale_x_discrete(label = c("adult", "child", "target child")) + 
  ylab("Number of Occurrences") + 
  ggtitle(label = "Top 20 Nouns: Sentential Position") + 
  #labs(caption = "sample size of 975 was selected based on group with lowest sample") + 
  annotate(x = )
theme(plot.caption = element_text(hjust = 0.5), legend.position = )

top_20_noun_find_sen_pos <- top_20_nouns %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(sentence_id) %>% 
  mutate(sentence_position = case_when(token_id == 1 ~ "initial",
                                       token_id == sentence_length ~ "final",
                                       TRUE ~ "medial")) %>% 
  filter(upos == "NOUN") %>% 
  select(recording_id, lemma, sentence_id, token_id,sentence_position, xds) %>% 
  filter(sentence_position %in% c('medial', 'final'))

token_id <- top_20_noun_find_sen_pos$token_id
token_id <- token_id - 1
sentence_id <- top_20_noun_find_sen_pos$sentence_id

previous_word <- tibble(token_id, sentence_id)
previous_word$token_id <- as.integer(token_id)

top_20_noun_prev_word <- left_join(previous_word, top_20_nouns_sent_context)

top_20_noun_prev_word_class_fun_content <- top_20_noun_prev_word %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  mutate(sentence_position = case_when(token_id == 1 ~ "initial",
                                       token_id == sentence_length ~ "final",
                                       TRUE ~ "medial")) %>% 
  select(recording_id, xds, lemma, upos, func_or_con )  %>% 
  group_by(recording_id, xds, lemma, func_or_con, upos) %>% 
  filter(!lemma %in% c('ta', 'hairy')) %>% 
  count(lemma) %>% 
  group_by(xds,func_or_con) %>% 
  summarise(total = sum(n),
            average = mean(n),
            sd = sd(n), sample = n_distinct(recording_id))

#nouns that occur initially 
top_20_noun_in_initial<- top_20_nouns %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(sentence_id) %>% 
  mutate(sentence_position = case_when(token_id == 1 ~ "initial",
                                       token_id == sentence_length ~ "final",
                                       TRUE ~ "medial")) %>% 
  filter(upos == "NOUN") %>% 
  select(recording_id, lemma, sentence_id, token_id,sentence_position, xds, sentence) %>% 
  filter(sentence_position %in% c('initial'))

ggplot(data = top_20_noun_prev_word_class, aes(x = fct_reorder(lemma,total,.desc = TRUE), y = total, color = xds)) + 
  geom_point()  +
  theme_bw()  +
  scale_y_continuous(breaks = seq(from = 0, to = 10, by = 1), limits = c(0,10)) + 
  xlab("Speech Addressee") + 
  #scale_x_discrete(label = c("adult", "child", "target child")) + 
  ylab("Total Number of Occurrences") + 
  ggtitle(label = "Functor Words Occuring Before Top 20 Nouns in Each Addressee Type") 
#labs(caption = "sample size of 975 was selected based on group with lowest sample") + 
#theme(plot.caption = element_text(hjust = 0.5))  



# top_20_nouns_sent_context_2 <- top_20_nouns_sent_context %>% 
#   filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
#   filter(xds %in% c('A', 'C', 'T')) %>% 
#   group_by(sentence_id) %>% 
#   mutate(sentence_position = case_when(token_id == 1 ~ "initial",
#                                        token_id == sentence_length ~ "final",
#                                        TRUE ~ "medial")) %>% 
# filter(sentence_position %in% c('medial', 'final'))
# 


ACLEW_min_sample_mlu <- ACLEW_word_by_classes %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(xds, recording_id) %>% 
  count(sentence)  %>%
  group_by(xds) %>% 
  summarise(total = sum(n))

sample <- min(ACLEW_min_sample_mlu$total)
sample <- as.numeric(sample)


#count mlu-w 
ACLEW_sample_for_mlu <- ACLEW_word_by_classes %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(xds) %>% 
  slice_sample(n = sample)   %>%
  select(recording_id, sentence_length,sentence_id, xds, lemma) %>% 
  group_by(recording_id, xds, sentence_id) %>% 
  summarise(sentence_length = mean(sentence_length)) %>% 
  group_by(recording_id, xds) %>% 
  summarise(mlu = mean(sentence_length)) 



ggplot(data = ACLEW_sample_for_mlu, aes(x = xds, y = mlu,color = xds)) +  #boxplot of avg # of utterances per addressee 
  geom_boxplot() + 
  geom_point(stat = "identity") + 
  #geom_point(stat = "identity") +                                                          #plots individual subjects as data point
  theme_bw() + 
  scale_y_continuous(breaks = seq(from = 0, to = 14, by = 2), limits = c(0,14)) + 
  xlab("Type of Addressee") + 
  scale_x_discrete(label = c("adult", "child", "target child")) + 
  ylab("Mean Length of Utterance") + 
  labs(caption = "sample size of 975 was selected based on group with lowest sample") + 
  theme(plot.caption = element_text(hjust = 0.5), legend.position = "none")
#sample size of 975 was selected based off lowest group 

ACLEW_corrected_dictionary <- ACLEW_word_by_classes %>%
  filter(!lemma %in% qdapDictionaries::interjections) %>%
  filter(lemma %in% qdapDictionaries::GradyAugmented) 

#find min sample for type-token ratio
ACLEW_min_sample_TTR <- ACLEW_corrected_dictionary %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(xds, recording_id, sentence) %>% 
  count(lemma)  %>%
  group_by(xds, recording_id) %>% 
  summarise(total = sum(n))


#calculate sample size 
sample <- min(ACLEW_min_sample_TTR$total)
sample <- as.numeric(sample)

#calculate TTR 
ACLEW_sample_for_TTR <- ACLEW_corrected_dictionary %>% 
  filter(!recording_id == "3749_scrubbed.eaf", !str_detect(lemma, pattern = "(shang)|(dax)|(banoona)|(daxes)")) %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'C', 'T')) %>% 
  group_by(xds) %>% 
  slice_sample(n = sample)   %>%
  select(recording_id,xds, token, lemma) %>% 
  group_by(recording_id, xds) %>% 
  count(lemma) %>% 
  group_by(xds) %>% 
  summarise(total_lemma = sum(n),
            distinct_lemma = n_distinct(lemma),
            TTR = mean(distinct_lemma/total_lemma), 
            sample = n_distinct(recording_id), 
            sd = sd(TTR))


# TTR = distinct_lemma/total_lemma)  
# group_by(xds) 
# summarise(avg_TTR = mean(TTR),
#           sd = sd(TTR))

ggplot(data = ACLEW_sample_for_TTR, aes(x = xds, y = TTR, fill = xds)) +  #boxplot of avg # of utterances per addressee 
  geom_bar(stat = "identity") + 
  #geom_point(stat = "identity") +                                                          #plots individual subjects as data point
  theme_bw() + 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .2), limits = c(0,1)) + 
  xlab("Type of Addressee") + 
  scale_x_discrete(label = c("adult", "child", "target child")) + 
  ylab("Type-Token Ratio ") + 
  labs(caption = "sample size of 893 was selected based on group with lowest sample") + 
  theme(plot.caption = element_text(hjust = 0.5), legend.position = "none")


#analyze and plot all functors 
functors_by_register <- ACLEW_word_by_classes %>% 
  filter(xds %in% c('T', 'C', 'A'), func_or_con == "functor") %>% 
  select(recording_id, lemma, token, upos, xds) %>% 
  group_by(recording_id,xds) %>% 
  count(lemma) %>% 
  group_by(lemma,xds) %>% 
  summarise(total = sum(n)) %>% 
  filter(!lemma %in% c('do', 'nose', 'scoot', 'daddy', 'athis', 'miss_thing', 'ta', 'Devon_LASTNAME', 'youare'))


ggplot(data = functors_by_register, aes(x = fct_reorder(lemma, total, .desc =TRUE), y = total, color = xds)) +
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = .65)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 400, by = 25), limits = c(0,400)) + 
  xlab("Functor Words") + 
  scale_x_discrete(expand = c(0,.9)) + 
  scale_colour_discrete(labels = c('adult', 'child','target child')) + 
  ylab("Number of Occurrence")

head_functor_by_register <-functors_by_register[order(functors_by_register$total, decreasing = TRUE),]
top_50_functor <- head(head_functor_by_register, 50)

ggplot(data = top_50_functor, aes(x = fct_reorder(lemma, total, .desc =TRUE), y = total, color = xds)) +
  geom_point() + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 45, vjust = .65)) + 
  scale_y_continuous(breaks = seq(from = 0, to = 400, by = 25), limits = c(0,400)) + 
  xlab("Top 50 Functor Words") + 
  scale_x_discrete(expand = c(0,.9)) + 
  scale_colour_discrete(labels = c('adult', 'child','target child')) + 
  ylab("Number of Occurrence")
