# nflwinbot
Bot that posts NFL win probabilities to the [@nfl_win_bot](https://twitter.com/nfl_win_bot) Twitter account from ESPN's API

Note: The code in the [tweet_winprobs.R](https://github.com/tuckerboynton22/nflwinbot/blob/main/tweet_winprobs.R) file runs on a DigitalOcean Droplet server, not on the GitHub Action in this repository. The GHA is set up here in case of emergency and only runs if the Droplet fails. For more detail on how to set up your own DigitalOcean Droplet, see the tutorial [here](https://www.marinedatascience.co/blog/2019/04/28/run-shiny-server-on-your-own-digitalocean-droplet-part-1/).

The [tweet_winprobs.R](https://github.com/tuckerboynton22/nflwinbot/blob/main/tweet_winprobs.R) file is largely borrowed from Ben Baldwin's [4th down bot](https://github.com/guga31bb/fourth_calculator) and uses an altered version of Thomas Mock's scrape_nfl_pbp() function from his [espnscrapeR](https://jthomasmock.github.io/espnscrapeR/) package, which pulls directly from the ESPN API.

On the server, the script runs every 15 seconds, scraping play-by-play information for any game occurring that day. The script identifies "big plays" (scores, turnovers, blocked punts, field goal attempts, and 4th down attempts) and tweets the ESPN win probability at that point in the game as well as a description of the play and corresponding win probability chart. After a play has been tweeted, its id is added to the file with old plays so the bot doesn't post it multiple times.

Below is an example of a tweet from the bot:
![](https://user-images.githubusercontent.com/70855507/151683608-c1e97e8d-9f64-455d-aab5-a8b16005417d.png)
