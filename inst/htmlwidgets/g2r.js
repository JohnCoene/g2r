HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    var view,
        chart;

    G2.track(false);

    return {

      renderValue: function(x) {

        // initialise
        initialOptions = x.opts;
        initialOptions.container = el.id;
        chart = new G2.Chart(initialOptions);

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
            var geom = eval("view." + layer.chart_type + "()");
  
            // loop through methods of layer
            layer.methods.forEach(function(method){
              var str = "geom." + method.name + "(" + JSON.stringify(method.args[0]) + "," + JSON.stringify(method.args[1]) + ")";
              eval(str);
            });
  
            if(layer.hasOwnProperty("animation"))
              geom.animate(layer.animation);
          }); 
        } else {
          chart.source(x.data);
          chart.facet(x.facet.type, x.facet.opts);
        }

        if(x.render)
          chart.render() // render
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
