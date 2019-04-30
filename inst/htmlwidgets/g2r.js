HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    var view,
        chart;
    G2.track(false);

    return {

      renderValue: function(x) {
        // theme
        if(x.hasOwnProperty('theme')){
          G2.Global.registerTheme('custom', x.theme);
          G2.Global.setTheme('custom')
        }

        // GLOBALS
        if(x.hasOwnProperty('font'))
          G2.Global.fontFamily = x.font;

        if(x.hasOwnProperty('renderer'))
          G2.Global.renderer = x.renderer;

        // initialise
        x.opts.container = el.id;
        chart = new G2.Chart(x.opts);

        var data = x.data;
        var opts = x.layers;

        if(!x.hasOwnProperty('facet')){
          opts.forEach(function(v){
            view = chart.view(v.layer);
            if(v.hasOwnProperty('data'))
              view.source(v.data);
            else
              view.source(data);
          });
        } else {
          chart.source(x.data, x.dataOpts);
          chart.facet(x.facet.type, x.facet.opts);
        }

        chart.render();
      },

      resize: function(width, height) {

      }

    };
  }
});
