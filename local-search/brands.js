// !preview r2d3 data = data.frame(id = c("1", "2", "3", "4", "5"), label = c("one", "two", "three", "four", "five"), value = c(300, 3, 100, 50, 80)), d3_version = 4

// Based on https://bl.ocks.org/mbostock/4063269

// Initialization
svg.attr("font-family", "sans-serif")
  .attr("font-size", "8")
  .attr("text-anchor", "middle");
    
var svgSize = 600;
var pack = d3.pack()
  .size([svgSize, svgSize])
  .padding(1);
    
var format = d3.format(",d");
var color = d3.scaleOrdinal(d3.schemeCategory20c);

var group = svg.append("g");

// Resize
r2d3.onResize(function(width, height) {
  var minSize = Math.min(width, height);
  var scale = minSize / svgSize;
  
  group.attr("transform", function(d) {
    return "" +
      "translate(" + (width - minSize) / 2 + "," + (height - minSize) / 2 + ")," +
      "scale(" + scale + "," + scale + ")";
  });
});

// Rendering
r2d3.onRender(function(data, svg, width, height, options) {
  var root = d3.hierarchy({children: data})
    .sum(function(d) { return d.value; })
    .each(function(d) {
      if (id = d.data.id) {
        var id, i = id.lastIndexOf(".");
        d.id = id;
        d.package = id.slice(0, i);
        d.class = id.slice(i + 1);
      }
    });

  var node = group.selectAll(".node")
    .data(pack(root).leaves())
    .enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });

  node.append("circle")
      .attr("id", function(d) { return d.id; })
      .attr("r", function(d) { return d.r; })
      .style("fill", function(d) { return "#fa8775"; })
      .attr('opacity', function(d) { return d.value / max_value(); });

  function max_value() {return d3.max(data, function (d) {return d.value; });} 
  node.append("text")
      .text(function(d) { return d.id })
      .style('font-size', function(d) { return d.r * 0.3; });

  r2d3.resize(width, height);
});