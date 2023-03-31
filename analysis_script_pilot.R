#pwr analysis 
#effect size based on Foushee 2020 d = 0.61; 
#paired t-test bc within subjects design for Exps 1 & 2 of Aim 2 
 
library(pwr)
sample <- pwr.t.test(d =0.61, sig.level = .05, power = .90, type = "paired", alternative = "two.sided" )

#calculate final sample size 
N = ceiling(sample$n)
num_groups = 2 
num_exp = 2 
add_participant_per_group = 10   #accounts for attrition rate 

N_across_exp = (N * num_groups) * num_exp
N_Aim2 = N_across_exp + (add_participant_per_group * num_groups * num_exp) 

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
#first, download the required library 
library(stringr)

#second,clean dataframe  
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

#so, we want to add two columns to the ACLEW_clean file: sentence id and sentence length
# the following two lines assign a unique ID for each sentence
ACLEW_all_anno_clean <- ACLEW_all_anno_clean %>% 
  mutate(sentence_id = row_number())                                                                                     #creates a sentence ID for each utterance using the row #  

#the next part is to determine the length of each utterance 
#some utterances have contractions. 
#for utterances w contracts, we want morphemes to be an individual token. So the contraction he's would be counted as he + 's 
#to do this, we have to replace all strings with a comma with a comma followed by space 
#then we move each token into a separate row 
ACLEW_tok_per_utt <- ACLEW_all_anno_clean %>% 
  mutate(annotation = str_replace_all(annotation, pattern = "'", replacement = " '"))                                    
ACLEW_tok_per_utt <- separate_rows(ACLEW_tok_per_utt, annotation, sep = " ")

#create a df that determines the length of each sentence 
length_per_utt <- ACLEW_tok_per_utt %>% 
  group_by(sentence_id) %>% 
  summarize(token_id = row_number(),
            sentence_length = n())

#combines df so that the ACLEW_tok_per_utt also contains sentence length column 
ACLEW_tok_per_utt <- bind_cols(ACLEW_tok_per_utt,length_per_utt, .name_repair = "unique")
ACLEW_tok_per_utt <- select(ACLEW_tok_per_utt, -sentence_id...11)
ACLEW_tok_per_utt <- rename(ACLEW_tok_per_utt, sentence_id = "sentence_id...12")


#next, we will use the udpipe. 
#udpipe prases text into tokens, lemma, and tags for parts of speech 
#first, we need to download an English model for it to correctly annotate our text 
#model is saved to folder 
library(udpipe)
dl <- udpipe_download_model(language = "english")
str(dl)

#then load model 
udmodel_english <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

#annotate annotations text 
#we are using the clean df bc udpipe requires sentence to annotate text correctly
ACLEW_anno_tagged <- udpipe_annotate(udmodel_english, x = ACLEW_all_anno_clean$annotation)
ACLEW_anno_tagged <- as.data.frame(ACLEW_anno_tagged)                                                 #creates df that separates each word in an utterance into a token, lemma, & identities the part of speech of the token 

#add function or content word column 
ACLEW_anno_tagged <- ACLEW_anno_tagged %>% 
  select(doc_id, sentence, token_id, token, lemma, upos, xpos, feats)  %>%
  mutate(func_or_con = case_when(upos == "ADJ" | upos == "NOUN"|upos == "VERB"|upos == "ADV" | upos == "INTJ"| upos == "PROPN" ~ "content",
                                 upos == "ADP" | upos == "AUX" | upos == "CCONJ"| upos == "DET" | upos == "NUM"|upos == "PART"| upos == "PRON"| upos == "SCONJ" ~ "function",
                                 TRUE ~"other")) %>% 
  filter(!upos == "PUNCT", !upos == "SYM" , !upos == "X", !upos == "NUM", !upos == "PROPN")

#ACLEW_anno_tagged does not contain participant_ID, xds, and participant. 
#this is where ACLEW_tok_per_utt comes in 
#first, make some changes to ACLEW_anno_tagged so that it contains columns that matches with ACLEW_tok_per_utt
#columns that the dfs will be match on are sentence_id and annotation 
#removes "doc" string from doc column  
#instead of doc column having the text, doc, + a number, it now contains the number
#the doc and sentences columns are renamed to sentence_id and annotation, respectively
ACLEW_anno_tagged$doc_id <- str_remove_all(ACLEW_anno_tagged$doc_id, pattern = "doc")
#ACLEW_anno_tagged <- rename(ACLEW_anno_tagged,  annotation= sentence)
ACLEW_anno_tagged <- rename(ACLEW_anno_tagged,sentence_id = doc_id)

#change sentence_id & token_id columns into integaers 
ACLEW_anno_tagged$sentence_id <- as.integer(ACLEW_anno_tagged$sentence_id)
ACLEW_anno_tagged$token_id <- as.integer(ACLEW_anno_tagged$token_id)


#df with all info (e.g., participant_id, xds, token, pos)
ACLEW_all_info_tagged <- right_join(ACLEW_tok_per_utt, ACLEW_anno_tagged) 
ACLEW_all_info_tagged <- ACLEW_all_info_tagged %>%                                   #remove 3749_scrubbed 
  filter(!recording_id == "3749_scrubbed.eaf")


#for MLU per addressee 
#calculate total sentences addressed to each addressee
ACLEW_N_utt <- ACLEW_all_info_tagged %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(xds, recording_id) %>% 
  count(sentence)  %>%
  group_by(xds) %>% 
  summarise(total = sum(n))

sample <- min(ACLEW_N_utt$total)
sample <- as.numeric(sample)


#calculate MLU-w at group level
ACLEW_MLU_per_xds_group <- ACLEW_all_info_tagged%>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(recording_id) %>% 
  slice_sample(n = sample)   %>%
  select(recording_id, sentence_length,sentence_id, xds) %>% 
  group_by(recording_id, xds, sentence_id) %>% 
  summarise(sentence_length = mean(sentence_length)) %>% 
  group_by(recording_id, xds) %>% 
  summarise(avg_mlu_id = mean(sentence_length), tot_utt = sample) %>% 
  group_by(xds) %>% 
  summarise(average = mean(avg_mlu_id), 
            N = n_distinct(recording_id),
            SD = sd(avg_mlu_id),
            SE = SD/sqrt(N),
            lower=average-SE,
            upper=average+SE)

 
  # 
  # summarise(avg_mlu_id = mean(sentence_length)) %>% 
  # group_by(xds) %>% 
  # summarise(average = mean(avg_mlu_id), 
  #           N = n_distinct(recording_id),
  #           SD = sd(avg_mlu_id),
  #           SE = SD/sqrt(N),
  #           lower=average-SE,
            # upper=average+SE)

ACLEW_MLU_per_xds_group <- left_join(ACLEW_MLU_per_xds_group, ACLEW_addresee_key)

#calculate MLU_w at individual level 
ACLEW_MLU_per_xds_individual <- ACLEW_all_info_tagged%>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(xds) %>% 
  slice_sample(n = sample)   %>%
  select(recording_id, sentence_length,sentence_id, xds) %>% 
  group_by(recording_id, xds, sentence_id) %>% 
  summarise(sentence_length = mean(sentence_length)) %>% 
  group_by(recording_id, xds) %>% 
  summarise(average = mean(sentence_length))

ACLEW_MLU_per_xds_individual <- left_join(ACLEW_MLU_per_xds_individual, ACLEW_addresee_key)

#calculate TTR at group level 

ACLEW_corrected <- ACLEW_all_info_tagged %>%
  filter(!lemma %in% qdapDictionaries::interjections) %>%
  filter(lemma %in% qdapDictionaries::GradyAugmented) 

#find min sample for type-token ratio
ACLEW_N_TTR <- ACLEW_corrected %>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(xds, recording_id, sentence) %>% 
  count(lemma)  %>%
  group_by(xds, recording_id) %>% 
  summarise(total = sum(n)) %>%
  group_by(recording_id) %>% 
  summarise(total = sum(total))


#calculate sample size 
sample <- min(ACLEW_N_TTR$total)
sample <- as.numeric(sample)

#calculate TTR on group level 
ACLEW_TTR_group <- ACLEW_corrected%>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(recording_id) %>% 
  slice_sample(n = sample) %>%  
  select(recording_id,xds, token, lemma) %>% 
  group_by(recording_id, xds) %>% 
  summarise(total_tokens = sample,                                                  #note to self: change column name of total_tokens to reflect sample size 
            total_distinct_word = sum(n_distinct(lemma)),
            TTR_id = total_distinct_word/total_tokens)  %>% 
  group_by(xds) %>% 
    summarise(TTR = mean(TTR_id),
              N = n_distinct(recording_id),
              SD = sd(TTR_id),
              SE = SD/sqrt(N),
              lower=TTR-SE,
             upper=TTR+SE) %>% 
 
  ACLEW_TTR_group <- left_join(ACLEW_TTR_group, ACLEW_addresee_key)

#calculate TTR on individual level 
ACLEW_TTR_individual <- ACLEW_all_info_tagged%>% 
  filter(!str_detect(sentence, pattern = "'")) %>% 
  filter(xds %in% c('A', 'K', 'T')) %>% 
  group_by(recording_id) %>% 
  slice_sample(n = sample) %>%  
  select(recording_id,xds, token, lemma) %>% 
  group_by(recording_id, xds) %>% 
  summarise(total_tokens = sample,
            total_distinct_word = sum(n_distinct(lemma)),
            TTR_id = total_distinct_word/total_tokens) 
ACLEW_TTR_individual <- left_join(ACLEW_TTR_individual, ACLEW_addresee_key)

 

#calculate total no of function and content words across all addressee
ACLEW_func_content_tot_xds <- ACLEW_all_info_tagged %>% 
  select(recording_id, xds, lemma, upos, func_or_con) %>% 
  group_by(recording_id, xds) %>%
  count(func_or_con) %>% 
  group_by(xds, func_or_con) %>% 
  summarise(average = mean(n))

## calculate top content words across addressee, focusing specifically on nouns and verbs 
ACLEW_top_content_ads <- ACLEW_corrected %>% 
  filter(func_or_con == "content") %>% 
  filter(upos %in% c("NOUN", "VERB")) %>% 
  filter(xds %in% c("A")) %>% 
  group_by(xds, lemma, upos) %>% 
  count(lemma) 


##calculate 


#calculate frequency of each function word, focusing specifically on aux, pronouns, and determiners 
ACLEW_func_freq <- ACLEW_corrected %>% 
  filter(func_or_con == "function") %>%    #filter for function words only 
  filter(xds %in% c("A","K","T")) %>%
  filter(upos %in% c("AUX", "PRON", "DET")) %>% 
  select(recording_id, xds, lemma, token, upos) %>% 
  group_by(recording_id, xds, lemma) %>%     
  count(lemma) %>% 
  filter(!lemma %in% c('babe', 'everything', 'every', 'anyone', 'everybody', 'which', 'anything', 'bath', 'another','bunny', 'daddy', 'forget', 'fourteen', 'hairy', 'may', 'might', 
                       'must', 'mine', 'myself', 'no', 'nose', 'nothing', 'quite', 'somebody', 
                       'someone', 'something', 'tan', 'such', 'yourself', 'ya', 'who', 'whatever', 
                       'what', 'tho', 'this', 'these', 'there', 'that', 'nobody', 'aright', 'everyone', 'some',
                       'each', 'ancestry', 'alright', 'bye', 'each', 'shave','could', 'should', 'any', 'all', 'those', 'would', 'herself', 'anybody', 'say')) %>%
  group_by(recording_id, lemma) %>% 
  summarise(frequency = sum(n))

#calculates frequency of each function word per xds 
ACLEW_func_xds <- ACLEW_corrected %>% 
  filter(func_or_con == "function") %>% 
  filter(xds %in% c("A","K","T")) %>%      #filter for function words only 
  filter(upos %in% c("AUX", "PRON", "DET")) %>% 
  select(recording_id, xds, lemma, token, upos) %>% 
  group_by(recording_id, xds, lemma) %>%     
  count(lemma) %>% 
  filter(!lemma %in% c('babe', 'everything', 'every', 'anyone', 'everybody', 'which', 'anything', 'bath', 'another','bunny', 'daddy', 'forget', 'fourteen', 'hairy', 'may', 'might', 
                       'must', 'mine', 'myself', 'no', 'nose', 'nothing', 'quite', 'somebody', 
                       'someone', 'something', 'tan', 'such', 'yourself', 'ya', 'who', 'whatever', 
                       'what', 'tho', 'this', 'these', 'there', 'that', 'nobody', 'aright', 'everyone', 'some',
                       'each', 'ancestry', 'alright', 'bye', 'each', 'shave','could', 'should', 'any', 'all', 'those', 'would', 'herself', 'anybody', 'say')) %>%
group_by(xds, lemma)

#combines func_freq & func_xds so that we have raw frequency of each function word & frequency of occurrence of each function word broken down by xds 
ACLEW_func_freq <- left_join(ACLEW_func_xds, ACLEW_func_freq) %>% 
  mutate(rel_freq = n/frequency)

ACLEW_func_freq$xds <- factor(ACLEW_func_freq$xds,  c('T', 'K', 'A')) 


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
