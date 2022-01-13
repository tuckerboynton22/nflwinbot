# Install the package from CRAN
install.packages("rtweet")

# Create a token containing your Twitter keys
rtweet::create_token(
  app = "nflwinbot",  # the name of the Twitter app
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# Example: post a tweet via the API
# The keys will are in your environment thanks to create_token()
rtweet::post_tweet(status = "This is a test tweet.")