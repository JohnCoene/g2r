HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    var view,
        chart,
        geom,
        coord,
        slider;

    var info = false;

    G2.track(false);

    return {

      renderValue: function(x) {

        // theme
        if(x.hasOwnProperty("theme")){
          G2.Global.registerTheme('custom', x.theme);
          G2.Global.setTheme('custom')
        }

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

            if(x.hasOwnProperty('axis'))
              x.axis.forEach(function(ax){
                if(ax.figure == layer.name || ax.figure == ""){
                  view.axis(ax.var, ax.opts);
                }
                  
              })

            if(x.hasOwnProperty("guides")){
              if(!info){
                x.guides.forEach(function(g){
                  eval("view.guide()." + g.type + "(" + JSON.stringify(g.opts) + ");");
                })
              }
              info = true;
            }
  
            // initialise layer
            geom = eval("view." + layer.chart_type + "()");
  
            // loop through methods of layer
            layer.methods.forEach(function(method){
              var arg1 = method.args[0];
              var arg2 = get_eval(method.args[1]);
              var str = "geom." + method.name + "(arg1, arg2)";
              eval(str);
            });
  
            if(layer.hasOwnProperty("animation"))
              geom.animate(layer.animation);

            if(x.hasOwnProperty("axis"))
              x.axis.forEach(function(ax){
                view.axis(ax.var, ax.opts);
              })
          }); 
        } else {
          chart.source(x.data, x.dataOpts);
          chart.facet(x.facet.type, x.facet.opts);
        }

        if(x.hasOwnProperty("legend"))
          chart.legend(x.legend);

        if(x.hasOwnProperty("tooltip"))
          chart.tooltip(x.tooltip);

        if(x.render)
          chart.render(); // render

        if(x.hasOwnProperty(slider)){
          chart.interact(x.slider.container, x.slider);
        }
      },

      resize: function(width, height) {

      }

    };
  }
});
