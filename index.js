require('dotenv').config();
const { Pool } = require('pg');
const { TwitterApi, TwitterApiV2Settings, ETwitterStreamEvent } = require("twitter-api-v2");


async function setupStream(client, postclient, dbclient) {
  const rules = await client.v2.streamRules();
  if (rules.data?.length) {
    await client.v2.updateStreamRules({
      delete: { ids: rules.data.map(rule => rule.id) },
    });
  }

  // Add our rules
  await client.v2.updateStreamRules({
    add: [{ value: '@joegame_' }],
  });

  const stream = await client.v2.searchStream({
    'tweet.fields': ['referenced_tweets', 'author_id'],
    expansions: ['referenced_tweets.id'],
  });
  // Enable auto reconnect
  stream.autoReconnect = true;
  stream.on(ETwitterStreamEvent.Data, async tweet => {
    // Ignore RTs or self-sent tweets
    const isARt = tweet.data.referenced_tweets?.some(tweet => tweet.type === 'retweeted') ?? false;
    if (isARt || tweet.data.author_id === process.env.TWITTER_JOEGAME_ID) {
      console.log('an err',tweet.data)
      return;
    }
    const arr = await crawlThread(tweet.data.id, client)
    await addThreadToDB(arr,dbclient)
    // Reply to tweet
    await postclient.v2.reply('Greetings, this thread has been successfully registere and will be added to the joegame desert. Thank you!', tweet.data.id);
    console.log('did it')
  });
  stream.on(ETwitterStreamEvent.ConnectionClosed, async _ => {
    dbclient.release()
  });
}

async function crawlThread(tweetid, client, arr = []) {
  const tw = await client.v2.tweets([tweetid], {
    'tweet.fields': ['author_id', 'in_reply_to_user_id', 'referenced_tweets', 'text', 'conversation_id', 'id', 'created_at']
  })
  // console.log(tw.data[0])
  arr.push(tw.data[0])
  if (tw.data[0].referenced_tweets) {
    const reply = tw.data[0].referenced_tweets.find(v => v.type == 'replied_to')
    if (reply) {
      return await crawlThread(reply.id, client, arr)
    }
  } else {
    return arr
  }
}

async function getNextConvoId(client) {
  const o = await client.query('SELECT max(convo_id) from tweet_threads;')
  return o ? o.rows[0].max + 1 : null
}

async function addThreadEntry(cid, pos, tid, dbclient) {
  return dbclient.query('INSERT INTO tweet_threads(convo_id,position,tweet_id) values ($1,$2,$3) ON CONFLICT DO NOTHING;',
    [cid, pos, tid])
}

async function addThreadToDB(arr, dbclient) {
  let convo_id = await getNextConvoId(dbclient)
  arr.reverse()
  if (convo_id) {
    for (let i = 0; i < arr.length; i++) {
      await saveTweetToDB(arr[i], dbclient)
      await addThreadEntry(convo_id,i,arr[i].id, dbclient)
    }
  }
}

async function saveTweetToDB(tweet, dbclient) {
  const q = `INSERT INTO tweets(tweet_id, author_id, created_at,tweet_text)
VALUES ($1,$2,$3,$4) ON CONFLICT DO NOTHING;`;
  const v = [tweet.id, tweet.author_id, tweet.created_at, tweet.text]
  let res = null
  try {
    res = await dbclient.query(q, v)

  } catch (err) {
    // Do something
    console.error(err)
  }
  return res

}

; (async () => {

  TwitterApiV2Settings.debug = false
  const pool = new Pool()
  const client = new TwitterApi({
    appKey: process.env.TWITTER_APP_KEY,
    appSecret: process.env.TWITTER_APP_SECRET,
    accessToken: process.env.TWITTER_ACCESS_TOKEN,
    accessSecret: process.env.TWITTER_ACCESS_TOKEN_SECRET
  })
  const clientOauth = await client.appLogin()
  const dbclient = await pool.connect()


  try {
    // setupStream(clientOauth, client)
    // await saveTweetToDB(tw.data[0], dbclient)
    // const a = await crawlThread('1465551245448470529', client)
    await setupStream(clientOauth,client,dbclient)
  } catch (er) {
    console.log(er)
  }
})().catch(err => console.log(err.stack))
