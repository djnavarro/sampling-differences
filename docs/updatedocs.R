# copy output files to docs folder. local versions of these files do not get
# published to the github repo until they are copied by running this script.
# this is deliberate, to ensure that the repo doesn't fill up with many copies
# of ouput files. run this script only when something has changed quite
# substantially and merits pushing an update to the online documentation.

file.copy(
  from = here::here("analysis", "analysis.html"),
  to = here::here("docs", "analysis.html")
)

file.copy(
  from = here::here("writeup", "model_description.pdf"),
  to = here::here("docs", "model_description.pdf")
)
