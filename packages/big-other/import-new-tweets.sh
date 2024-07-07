#!/usr/bin/env sh
scp mik@100.98.141.12:joegame-twitter-bot/tweets.db /tmp/new-tweets.db
sqlite3 assets/jdb.db <<< "ATTACH DATABASE \"/tmp/new-tweets.db\" as in_db;
INSERT INTO tweets(created_at, author_id, tweet_text, media_poll, tweet_id) SELECT  created_at, author_id, tweet_text, media_poll, tweet_id FROM in_db.tweets WHERE TRUE ON CONFLICT DO NOTHING;
INSERT INTO tweet_threads(position, convo_id, tweet_id) SELECT  position, convo_id, tweet_id FROM in_db.tweet_threads WHERE TRUE ON CONFLICT DO NOTHING;
INSERT INTO tweeters(author_id, username) SELECT  author_id, username FROM in_db.tweeters WHERE TRUE ON CONFLICT DO NOTHING;"
rm /tmp/new-tweets.db
