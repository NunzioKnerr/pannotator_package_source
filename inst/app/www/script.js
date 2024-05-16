$( document ).ready(function() {

});

$(document).on('shiny:connected', function(event) {
  Shiny.addCustomMessageHandler('removeLastFeature', function(layerId) {
    var map = leafletData.getMap('map');
    var drawnItems = map.editTools.featuresLayer;
    var layerToRemove = drawnItems.getLayer(layerId);
    if (layerToRemove) {
      drawnItems.removeLayer(layerToRemove);
    }
  });
});

