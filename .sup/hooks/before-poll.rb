#if (@last_fetch || Time.at(0)) < Time.now - 120
  say "Running offlineimap..."
  system("offlineimap -q -u Noninteractive.Basic > /dev/null 2>&1")
  say "Finished offlineimap run."
#end
@last_fetch = Time.now
