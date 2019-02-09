library(rtweet)

# 24hrs of streaming (less 5 mins), do not parse
#rt <- stream_tweets(q='cambridge,english,ielts', timeout=(60*60*24)-(5*60), file_name='~/Downloads/english24_1.json', parse=F)
rt <- stream_tweets(q='cambridge,english,ielts', timeout=(60*60*24)-(5*60), file_name='~/Downloads/english24_2.json', parse=F)

# then parse
#rt <- parse_stream('~/Downloads/english24_1.json')
rt <- parse_stream('~/Downloads/english24_2.json')

# select columns
tweets <- rt[,c(1:5,8:9,11:12,30,72,75:77,81:82,85)]
if (!colnames(tweets)[1]=='user_id') { print('First column != user_id') }
if (!colnames(tweets)[ncol(tweets)]=='account_lang') { print('First column != account_lang') }

# replace speech marks and newlines in text
tweets$screen_name <- gsub('\\n', ' . ', gsub('"', "'", tweets$screen_name))
tweets$location <- gsub('\\n', ' . ', gsub('"', "'", tweets$location))
tweets$text <- gsub('\\n', ' . ', gsub('"', "'", tweets$text))

# save and compress
#write.table(tweets, '~/Downloads/english24_1.csv', sep=';', row.names=F)
#system('tar -zcvf ~/Downloads/english24_1.csv.tgz ~/Downloads/english24_1.csv')
write.table(tweets, '~/Downloads/english24_2.csv', sep=';', row.names=F)
system('tar -zcvf ~/Downloads/english24_2.csv.tgz ~/Downloads/english24_2.csv')
