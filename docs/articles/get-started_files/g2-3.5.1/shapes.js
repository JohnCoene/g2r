// triangle for interval geom
G2.Shape.registerShape('interval', 'triangle', {
  getPoints(cfg) {
    const x = cfg.x;
    const y = cfg.y;
    const y0 = cfg.y0;
    const width = cfg.size;
    return [
      { x: x - width / 2, y: y0 },
      { x: x, y: y },
      { x: x + width / 2, y: y0 }
    ]
  },
  draw(cfg, group) { // 自定义最终绘制
    const points = this.parsePoints(cfg.points); // 将0-1空间的坐标转换为画布坐标
    const polygon = group.addShape('polygon', {
      attrs: {
        points: [
          [ points[0].x, points[0].y ],
          [ points[1].x, points[1].y ],
          [ points[2].x, points[2].y ]
        ],
        fill: cfg.color
      }
    });
    return polygon; // !必须返回 shape
  }
});

G2.Shape.registerShape('point', 'cloud', {
  drawShape: function drawShape(cfg, container) {
    var attrs = getTextAttrs(cfg);
    return container.addShape('text', {
      attrs: _.assign(attrs, {
        x: cfg.x,
        y: cfg.y
      })
    });
  }
});