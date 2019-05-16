HTMLWidgets.widget({

  name: 'g2r',

  type: 'output',

  factory: function(el, width, height) {

    var view,
        opts,
        chart;
    var views = [];
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

        opts = x.layers;

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

        if(!x.hasOwnProperty('facet'))
          chart.scale(x.dataOpts)
        
        if(x.hasOwnProperty("allAxes"))
          chart.axis(false);

        if(!x.hasOwnProperty('facet')){
          opts.forEach(function(v){
            view = chart.view(v.layer);
            
            if(v.hasOwnProperty('data'))
              view.source(v.data);
            else
              view.source(x.data);

            if(x.hasOwnProperty("axes"))
              view.axis(x.axes);
            
            views.push(view); // add view to array for proxy
          });
        } else {
          chart.source(x.data, x.dataOpts);
          chart.facet(x.facet.type, x.facet.opts);
        }

        if(x.hasOwnProperty("legend"))
          x.legend.forEach(function(leg){
            chart.legend(leg[0], leg[1]);
          })

        if(x.hasOwnProperty("tooltip"))
          chart.tooltip(x.tooltip);

        if(x.hasOwnProperty("render"))
          chart.render();

        if(x.hasOwnProperty("brush"))
          chart.interact("brush");

        if(x.hasOwnProperty("slider"))
          chart.interact("slider", x.slider);

        if(x.hasOwnProperty("zoom"))
          chart.interact("zoom", x.zoom);

        if(x.hasOwnProperty("drag"))
          chart.interact("drag", x.drag);

        if (HTMLWidgets.shinyMode) {
          if(x.hasOwnProperty("plotCallbacks")){
            chart.on('plotenter', function(e){
              Shiny.onInputChange(el.id + '_plot_enter', {x: e.x, y: e.y});
            });
            chart.on('plotleave', function(e){
              Shiny.onInputChange(el.id + '_plot_leave', {x: e.x, y: e.y});
            });
            chart.on('plotclick', function(e){
              Shiny.onInputChange(el.id + '_plot_click', {x: e.x, y: e.y});
            });
          }

          if(x.hasOwnProperty("getSnapRecords")){
            x.getSnapRecords.forEach(function(cb){
              chart.on(cb.on, function(ev){
                console.log(ev);
                var records = chart.getSnapRecords({x: ev.x, y: ev.y});
                console.log(records);
                Shiny.onInputChange(el.id + '_' + cb.action, records);
              }); 
            })
          }
        }
      },

      resize: function(width, height) {

      },

      getChart: function(){
        return chart;
      },

      getView: function(){
        return views;
      },

    };
  }
});

function getInstance(id){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var g2;

  if (typeof htmlWidgetsObj != 'undefined') {
    g2 = htmlWidgetsObj.getChart();
  }

  return(g2);
}

function getViews(id){

  var htmlWidgetsObj = HTMLWidgets.find("#" + id);

  var v;

  if (typeof htmlWidgetsObj != 'undefined') {
    v = htmlWidgetsObj.getView();
  }

  return(v);
}

if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler('clear',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.clear();
      }
  }); 

  Shiny.addCustomMessageHandler('destroy',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.destroy();
      }
  }); 

  Shiny.addCustomMessageHandler('render',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.render();
      }
  }); 

  Shiny.addCustomMessageHandler('repaint',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.repaint();
      }
  });
  
  Shiny.addCustomMessageHandler('changeData',
    function(data) {
      var views = getViews(data.id);
      if (typeof views != 'undefined') {
        views.forEach(function(v, i){
          if(data.figures = "*")
            v.changeData(data.data)
          else if(data.figures.includes(i))
            v.changeData(data.data)
        })
      }
  });

  Shiny.addCustomMessageHandler('changeSize',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.changeSize(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('changeHeight',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.changeHeight(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('changeWidth',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.changeWidth(data.opts);
      }
  });

  Shiny.addCustomMessageHandler('toDataURL',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.toDataURL();
      }
  });

  Shiny.addCustomMessageHandler('downloadImage',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.downloadImage(data.name);
      }
  });
  
  Shiny.addCustomMessageHandler('showTooltip',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.showTooltip(data.tooltip);
      }
  });

  Shiny.addCustomMessageHandler('hideTooltip',
    function(data) {
      var chart = getInstance(data.id);
      if (typeof chart != 'undefined') {
        chart.hideTooltip();
      }
  });

  Shiny.addCustomMessageHandler('hide',
    function(data) {
      var views = getViews(data.id);
      if (typeof views != 'undefined') {
        views.forEach(function(v, i){
          if(data.figures = "*")
            v.hide()
          else if(data.figures.includes(i))
            v.hide()
        })
      }
  });

  Shiny.addCustomMessageHandler('show',
    function(data) {
      var views = getViews(data.id);
      if (typeof views != 'undefined') {
        views.forEach(function(v, i){
          if(data.figures = "*")
            v.show()
          else if(data.figures.includes(i))
            v.show()
        })
      }
  });
}