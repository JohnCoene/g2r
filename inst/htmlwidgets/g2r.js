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

        x.layers.forEach(function(layer){

          // required
          view = chart.view();
          view.source(x.data);

          // initialise layer
          var geom = eval("view." + layer.chart_type + "()");

          console.log(layer);

          // loop through methods of layer
          layer.methods.forEach(function(method){
            var str = "geom." + method.name + "(" + JSON.stringify(method.arg) + ")";
            eval(str);
          });
        }); 

        if(x.render)
          chart.render() // render
      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
