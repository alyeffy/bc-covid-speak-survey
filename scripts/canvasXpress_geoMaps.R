library(canvasXpress)
library(htmlwidgets)
library(jsonlite)

ha  <- read_json('data/map_HA_boundaries.json')
ha.events <- JS("{
                'mousemove' : function(o, e, t) {
                    if (o) {
                        t.showInfoSpan(e, '<b>' + o.z.HLTH_AUTHORITY_NAME[0] + '</b> <i>(' + o.z.HLTH_AUTHORITY_CODE[0] + ')</i>');
                    };
                }}")

canvasXpress(
  data            = FALSE,
  graphType       = "Map",
  title           = "Health Authority Boundaries",
  colorBy         = "HLTH_AUTHORITY_NAME",
  showLegendTitle = FALSE,
  topoJSON        = ha,
  events          = ha.events
)


lha <- read_json('data/map_LHA_boundaries.json')
lha.events <- JS("{
                'mousemove' : function(o, e, t) {
                    if (o) {
                        console.log(o);
                        t.showInfoSpan(e, '<b>' + o.z.LOCAL_HLTH_AREA_NAME[0] + '</b> <i>(' + o.z.LOCAL_HLTH_AREA_CODE[0] + ')</i><br/>' +
                                          'HA: ' + o.z.HLTH_AUTHORITY_NAME[0] + ' <i>(' + o.z.HLTH_AUTHORITY_CODE[0] + ')</i>');
                    };
                }}")

canvasXpress(
  data       = FALSE,
  graphType  = "Map",
  title      = "Local Health Authority Boundaries",
  colorBy    = "LOCAL_HLTH_AREA_NAME",
  showLegend = FALSE,
  topoJSON   = lha,
  events     = lha.events
)
