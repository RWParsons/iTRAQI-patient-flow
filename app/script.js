var mapsPlaceholder = [];
L.Map.addInitHook(function () {
   mapsPlaceholder.push(this); // Use whatever global scope variable you like.
});

function open_popup(id) {
   // console.log('open popup for ' + id);
   // console.log('open popup for ' + mapsPlaceholder.constructor.name);
   // console.log('open popup for ' + mapsPlaceholder.map(s=>s.id));
   mapsPlaceholder.find(x => x.id === 'map').eachLayer(function(l) {
   //mapsPlaceholder['map'].eachLayer(function(l) {
      if (l.options && l.options.layerId == id) {
         l.openPopup();
      }
   });
}

window.LeafletWidget.methods.setStyle = function(category, layerId, style){
      var map = this;
      if (!layerId){
        return;
      } else if (!(typeof(layerId) === 'object' && layerId.length)){ // in case a single layerid is given
        layerId = [layerId];
      }
    
      //convert columnstore to row store
      style = HTMLWidgets.dataframeToD3(style);
      //console.log(style);
    
      layerId.forEach(function(d,i){
        var layer = map.layerManager.getLayer(category, d);
        if (layer){ // or should this raise an error?
          layer.setStyle(style[i]);
        }
      });
    };
	
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
	return;
  } else if (!(typeof(layerId) === 'object' && layerId.length)){ // in case a single layerid is given
	layerId = [layerId];
  }

  layerId.forEach(function(d,i){
	var layer = map.layerManager.getLayer(category, d);
	if (layer){ // or should this raise an error?
	  layer.unbindTooltip();
	  // the object subsetting to get the integer array and casting to string is what I added
	  layer.bindTooltip(label.label[i].toString());
	}
  });
};