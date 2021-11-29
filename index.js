const { TwitterApi, TwitterApiV2Settings, ETwitterStreamEvent} = require("twitter-api-v2");


async function setupStream(client, postclient) {
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
    if (isARt || tweet.data.author_id === "@joegame_") {
      return;
    }

    // Reply to tweet
    await postclient.v2.reply('u talking to me???', tweet.data.id);
  });
}

(async () => {

  TwitterApiV2Settings.debug = true
  const client = new TwitterApi({
    appKey: process.env.TWITTER_APP_KEY,
    appSecret: process.env.TWITTER_APP_SECRET,
    accessToken: process.env.TWITTER_ACCESS_TOKEN,
    accessSecret: process.env.TWITTER_ACCESS_TOKEN_SECRET
  })
  const clientOauth = await client.appLogin()
  // Home timeline is available in v1 API, so use .v1 prefix
  try{
    setupStream(clientOauth, client)
  } catch(er){
    console.log(error)
  }
  // const sendtw = await client.v2.tweet("how do you do22")
  // console.log(sendtw)
  // Current page is in homeTimeline.tweets
  // console.log(homeTimeline.tweets.length, 'fetched.');
  // console.log(homeTimeline.tweets[0], 'fetched.');
})()

