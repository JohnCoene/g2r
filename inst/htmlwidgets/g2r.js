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
          view = chart.view();
          view.source(x.data);
          eval("view." + layer.chart_type + "()").position(layer.position);
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
