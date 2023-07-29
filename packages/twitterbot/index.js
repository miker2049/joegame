require("dotenv").config();
// const { Pool } = require('pg');
const sqlite3 = require("sqlite3");
const {
    TwitterApi,
    TwitterApiV2Settings,
    ETwitterStreamEvent,
} = require("twitter-api-v2");
// const TWEET_FIELDS=

/*
 * Initiate a filtered stream and listen for events
 */
async function setupStream(client, postclient, dbclient) {
    const rules = await client.v2.streamRules();
    if (rules.data?.length) {
        await client.v2.updateStreamRules({
            delete: { ids: rules.data.map((rule) => rule.id) },
        });
    }

    // Add our rules
    await client.v2.updateStreamRules({
        add: [{ value: "@joegame_" }],
    });

    const stream = await client.v2.searchStream({
        "tweet.fields": ["referenced_tweets", "author_id"],
        expansions: ["referenced_tweets.id"],
    });
    // Enable auto reconnect
    stream.autoReconnect = true;
    stream.on(ETwitterStreamEvent.Data, async (tweet) => {
        // Ignore RTs or self-sent tweets
        const isARt =
            tweet.data.referenced_tweets?.some(
                (tweet) => tweet.type === "retweeted"
            ) ?? false;
        if (isARt || tweet.data.author_id === process.env.TWITTER_JOEGAME_ID) {
            console.log("an err", tweet.data);
            return;
        }
        const arr = await crawlThread(tweet.data.id, client);

        if (arr) {
            if (arr.length > 0) {
                addThreadToDB(arr, dbclient);
                // Reply to tweet
                await postclient.v2.reply(
                    "Greetings, this thread has been successfully registered and will be added to the joegame desert. Thank you!",
                    tweet.data.id
                );
                console.log("did it");
            }
        }
    });
    stream.on(ETwitterStreamEvent.ConnectionClosed, async (_) => {
        // dbclient.release()
    });
}

async function getUsername(author_id, client) {
    const out = await client.v2.user(author_id);
    return out.data.username;
}

async function crawlThread(tweetid, client, arr = []) {
    const tw = await client.v2.tweets([tweetid], {
        "tweet.fields": [
            "author_id",
            "in_reply_to_user_id",
            "referenced_tweets",
            "text",
            "conversation_id",
            "id",
            "created_at",
        ],
    });
    // console.log(tw.data[0])

    //already been added

    if (tw.data[0].author_id == process.env.TWITTER_JOEGAME_ID) {
        console.log("an err", tw.data[0]);
        return [];
    }

    tw.data[0].username = await getUsername(tw.data[0].author_id, client);
    arr.push(tw.data[0]);
    if (tw.data[0].referenced_tweets) {
        const reply = tw.data[0].referenced_tweets.find(
            (v) => v.type == "replied_to"
        );
        if (reply) {
            return await crawlThread(reply.id, client, arr);
        } else {
            return arr;
        }
    } else {
        return arr;
    }
}

async function getNextConvoId(client) {
    return new Promise((res, rej) => {
        client.get(
            "SELECT max(convo_id) as MM from tweet_threads;",
            (err, row) => {
                res(row ? row.MM + 1 : null);
                // return o ? o.rows[0].max + 1 : null
            }
        );
    });
}

function addThreadEntry(cid, pos, tid, dbclient) {
    return dbclient.run(
        "INSERT INTO tweet_threads(convo_id,position,tweet_id) values (?,?,?) ON CONFLICT DO NOTHING;",
        [cid, pos, tid]
    );
}

function insertTweeter(author_id, username, dbclient) {
    return dbclient.run(
        "INSERT INTO tweeters(author_id, username) values (?,?) ON CONFLICT DO NOTHING",
        author_id,
        username
    );
}

function saveTweetToDB(tweet, dbclient) {
    const q = `INSERT INTO tweets(tweet_id, author_id, created_at,tweet_text)
VALUES (?,?,?,?) ON CONFLICT DO NOTHING;`;
    const v = [tweet.id, tweet.author_id, tweet.created_at, tweet.text];
    let res = null;
    try {
        res = dbclient.run(q, v);
    } catch (err) {
        // Do something
        console.error(err);
    }
    return res;
}

async function addThreadToDB(arr, dbclient) {
    arr.reverse();
    for (let i = 0; i < arr.length; i++) {
        // insertTweeter(arr[i].author_id, arr[i].username, dbclient)
        saveTweetToDB(arr[i], dbclient);
        addThreadEntry(arr[i].conversation_id, i, arr[i].id, dbclient);
    }
}

(async () => {
    TwitterApiV2Settings.debug = false;
    // const pool = new Pool()
    const client = new TwitterApi({
        appKey: process.env.TWITTER_APP_KEY,
        appSecret: process.env.TWITTER_APP_SECRET,
        accessToken: process.env.TWITTER_ACCESS_TOKEN,
        accessSecret: process.env.TWITTER_ACCESS_TOKEN_SECRET,
    });
    const clientOauth = await client.appLogin();
    const dbclient = new sqlite3.Database(process.env.BO_PATH);

    if (true) {
        const bs = [
            "1648754406655942662",
            "1648366156460294145",
            "1648343484405891072",
            "1647659408904601604",
            "1647643372784349185",
            "1646653124864606210",
            "1646708718233620481",
            "1646753840715243523",
            "1646745140495622145",
            "1646600328949780480",
            "1646574591568142337",
            "1645862450607149059",
            "1645994559200022530",
            "1645560570484256768",
            "1645434389675794434",
            "1645043844591878144",
            "1644630117661003780",
        ];
        console.log(
            "detecting command line usage, no bot mode",
            process.env.TWEET_DB
        );
        for (const f of bs) {
            console.log(f);
            const arr = await crawlThread(f, client);
            if (arr.length > 0) {
                addThreadToDB(arr, dbclient);
                console.log(arr);
                console.log("did it, commandLine style");
            }
        }
        dbclient.close();
        return;
    } else {
        try {
            await setupStream(clientOauth, client, dbclient);
        } catch (er) {
            console.log(er);
        }
    }
})().catch((err) => console.log(err.stack));
