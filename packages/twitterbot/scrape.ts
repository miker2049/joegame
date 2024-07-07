// -*- lsp-enabled-clients: (deno-ls); -*-

import { DB } from "https://deno.land/x/sqlite/mod.ts";
import { Client } from "npm:twitter-api-sdk";

const db = new DB("jdb.db");
// (async () => {
//     try {
//         const sampleStream = await twitterClient.tweets.sampleStream({
//             //A comma separated list of fields to expand
//             expansions: ["author_id"],

//             //The number of minutes of backfill requested
//             backfill_minutes: 5,

//             //A comma separated list of Tweet fields to display.
//             "tweet.fields": ["created_at", "conversation_id"],
//         });
//         console.dir(sampleStream, {
//             depth: null,
//         });
//     } catch (error) {
//         console.log(error);
//     }
// })();
const client = new Client(Deno.env.get("TWITTER_BEARER_TOKEN"));

async function main() {
    try {
        let count = 0;
        const stream = client.tweets.sampleStream({
            "tweet.fields": ["author_id", "created_at", "conversation_id"],
            expansions: ["author_id"],
        });

        for await (const tweet of stream) {
            console.log(count, tweet.data);
            addTweetToDB(tweet.data);
            count = count + 1;
            if (count > 10000) {
                break;
            }
        }
    } catch (error) {
        console.log(error);
    }
}

function addTweetToDB(row) {
    const { author_id, conversation_id, created_at, id, text } = row;
    // Insert into the `tweets` table
    const tweetResult = db.query(
        `INSERT OR IGNORE INTO tweets (created_at, author_id, tweet_text, tweet_id) VALUES (?, ?, ?, ?)`,
        [created_at, author_id, text, id]
    );
    // get max convo_id
    const maxConvoPos = db.query<[number]>(
        "SELECT MAX(position) FROM tweet_threads WHERE convo_id = ?",
        [conversation_id]
    )[0][0];
    // Insert into the `tweet_threads` table
    db.query(
        `INSERT OR IGNORE INTO tweet_threads (position, convo_id, tweet_id) VALUES (?, ?, ?)`,
        [maxConvoPos ? maxConvoPos + 1 : 0, conversation_id, id]
    );
}
main();
