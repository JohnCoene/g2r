HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    var view,
        chart,
        geom,
        coord;

    G2.track(false);

    return {

      renderValue: function(x) {

        // initialise
        initialOptions = x.opts;
        initialOptions.container = el.id;
        chart = new G2.Chart(initialOptions);

        // Coordinates
        if(x.hasOwnProperty("coord"))
          coord = chart.coord(x.coord.type, x.coord.opts);

        if(x.hasOwnProperty("coordRotate"))
          coord.rotate(x.coordRotate);

        if(x.hasOwnProperty("coordScale"))
          coord.scale(x.coordScale[0], x.coordScale[1]);

        if(x.hasOwnProperty("coordReflect"))
          coord.reflect(x.coordReflect);

        if(x.hasOwnProperty("coordTranspose"))
          coord.transpose();

        if(!x.hasOwnProperty("facet")){
          x.layers.forEach(function(layer){

            // required
            view = chart.view();
            
            // source data 
            if(layer.hasOwnProperty('data'))
              view.source(layer.data);
            else
              view.source(x.data);
  
            // initialise layer
            geom = eval("view." + layer.chart_type + "()");
  
            // loop through methods of layer
            layer.methods.forEach(function(method){
              var str = "geom." + method.name + "(" + JSON.stringify(method.args[0]) + "," + JSON.stringify(method.args[1]) + ")";
              eval(str);
            });
  
            if(layer.hasOwnProperty("animation"))
              geom.animate(layer.animation);
          }); 
        } else {
          chart.source(x.data, x.dataOpts);
          chart.facet(x.facet.type, x.facet.opts);
        }

        if(x.hasOwnProperty("legend"))
          chart.legend(x.legend);

        if(x.render)
          chart.render(); // render
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
