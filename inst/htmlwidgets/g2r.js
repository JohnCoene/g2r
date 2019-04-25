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
          
          // source data 
          if(layer.hasOwnProperty('data'))
            view.source(layer.data);
          else
            view.source(x.data);

          // initialise layer
          var geom = eval("view." + layer.chart_type + "()");

          // loop through methods of layer
          layer.methods.forEach(function(method){
            console.log(JSON.stringify(method.args[1]));
            var str = "geom." + method.name + "(" + JSON.stringify(method.args[0]) + "," + JSON.stringify(method.args[1]) + ")";
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
