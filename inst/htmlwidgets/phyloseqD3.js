HTMLWidgets.widget({

    name: 'phyloseqD3',
    type: 'output',

    initialize: function(el, width, height) {
	// Build the main visualization canvas
	var wrap = d3.select(el).append("svg")
	    .attr("width", width)
	    .attr("height", height - 30)
	    .style("-webkit-backface-visibility", "hidden");

	// Shifts whole graphic down and across by radius r, so we see the whole phylo
	var vis = wrap.append("g")
	    .attr("transform", "translate(0," + .05 * height + ")");

	// Map the raw data to xy coordinates of phylogenetic tree
	var cluster = d3.layout.cluster()
	    .size([width, height])
	    .sort(null)
	    .value(function(d) { return d.length; })
	    .children(function(d) { return d.branchset; })
	    .separation(function(a, b) { return 1; });

	// Return instance data
	return {
	    width: width,
	    height: height,
	    wrap: wrap,
	    vis: vis,
	    cluster: cluster
	};
    },

    resize: function(el, width, height, instance) {
	// Function to react to window resizing
	//
	// Args:
	//   el: The HTML element containing the visualization
	//   width: The new width of the window
	//   height: The new height of the window
	//   instance: The instance data, from initialization or the last
	//     resizing
	//
	// Returns:
	//   instance: The instance data updated with the new window sizes. Will
	//     update the plot upon refreshing the page (can't resize
	//     automatically, because we need the problem data to compute
	//     heights along the tree.

	// Build the main visualization canvas
	var wrap = d3.select(el).append("svg")
	    .attr("width", width)
	    .attr("height", height - 30)
	    .style("-webkit-backface-visibility", "hidden");

	// Shifts whole graphic down and across by radius r, so we see the whole phylo
	var vis = wrap.append("g")
	    .attr("transform", "translate(0," + .05 * height + ")");

	// Map the raw data to xy coordinates of phylogenetic tree
	var cluster = d3.layout.cluster()
	    .size([width, height])
	    .sort(null)
	    .value(function(d) { return d.length; })
	    .children(function(d) { return d.branchset; })
	    .separation(function(a, b) { return 1; });

	return {
	    width: width,
	    height: height,
	    wrap: wrap,
	    vis: vis,
	    cluster: cluster
	};
    },

    renderValue: function(el, x, instance) {
	// Render the phylogenetic tree
	//
	// Args:
	//   el: The HTML element containing the visualization
	//   x: The problem data input by R
	//   instance: The instance data, from initialization or the last
	//     resizing

	i=0;
	var tree = JSON.parse(x.tree_string);

	// Build the dropdown menu
	var select = d3.select(el)
	    .append('select')
	    .attr('class','select')
	    .on('change', onchange)

	var opts = Object.keys(tree.counts);
	// Populate the dropdown menu with the groups we aggregated by
	var options = select
	    .selectAll('option')
	    .data(opts).enter()
	    .append('option')
	    .text(function (d) {
		return d; });

	// Recursion that allows collapsing subtrees onto their parent. Invisible
	// nodes have _branchset option on, visible ones have branchset
	function collapse(d) {
	    if (d.branchset) {
		d._branchset = d.branchset;
		d._branchset.forEach(collapse);
		d.branchset = null;
	    }
	}

	// Function to find endpoints to draw edges
	function project(d) {
	    return [canvasScaleX(d.x), d.y];
	}

	// Function to draw the edges
	function step(d) {
	    var s = project(d.source),
		m = project({x: d.target.x, y: d.source.y}),
		t = project(d.target),
		r = d.source.y,
		sweep = d.target.x > d.source.x ? 1 : 0;
	    return (
		"M" + s[0] + "," + s[1] +
		    "A" + instance.width / 2 + "," + instance.width / 2 + " 0 0," + sweep + " " + m[0] + "," + m[1] +
		    "L" + t[0] + "," + t[1]);
	}

	// Scales to adapt phylo size depending on depth of longest edge
	var canvasScaleY = d3.scale.linear()
	    .domain([0, x.max_depth])
	    .range([0, .80 * instance.height]);
	var rScale = d3.scale.log()
	    .domain([1, x.max_group_counts])
	    .range([2, 10]);
	var edgeColorScale = d3.scale.linear()
	    .domain([0, 1])
	    .range(["indianred", "steelblue"]);
	var edgeWidthScale = d3.scale.linear()
	    .domain([0, 1])
	    .range([1, 10]);
	var canvasScaleX = d3.scale.linear()
	    .domain([0, d3.max(instance.cluster.nodes(tree), function(d) { return(d.x); })])
	    .range([.05 * instance.width, .95 * instance.width]); // replace with the width of the canvas

	// Ensure lengths on screen represent lengths in true tree edges
	function phylo(n, offset) {
	    if (n.length != null) offset += canvasScaleY(n.length);
	    n.y = offset;
	    if (n.children)
		n.children.forEach(function(n) {
		    phylo(n, offset);
		});
	}

	// Update called recursively upon clicks, to collapse the tree
	//
	//   Args:
	//     source: The node below which to update / collapse
	function update(source) {

	    // Compute the new cluster layout.
	    var nodes = instance.cluster.nodes(tree);
	    phylo(nodes[0], 0);
	    nodes.reverse();
	    var links = instance.cluster.links(nodes);

	    // Associate data from JSON string to the ndoes
	    var node = instance.vis.selectAll("g.node")
		.data(nodes.filter(function(n) {
		    return n.x !== undefined; }),
		      function(d) { return d.id || (d.id = ++i); });

	    // Set properties of node before any clicking / collapsing
	    var nodeEnter = node.enter().append("g")
		.attr("class", "node")
		.attr("transform", function(d) {
		    return "translate(" + canvasScaleX(source.x0) + "," + source.y0 + ")"
		})
		.attr("r", function(d) {
		    return rScale(1.0 + d.counts[selectValue]);
		})
		.on("click", click)
		.on("mouseover", function(d) {
		    var g = d3.select(this); // The node
		    // The class is used to remove the additional text later
		    var node_name = g.append('text')
			.classed('node_name', true)
			.text(d.name);
		    var node_counts = g.append('text')
			.classed('node_counts', true)
			.attr("y", -10)
			.attr("fill", "#990066")
			.text(d.counts[selectValue]);
		})
		.on("mouseout", function() {
		    // Remove the info text on mouse out.
		    d3.select(this).select('text.node_name').remove();
		    d3.select(this).select('text.node_counts').remove();
		});

	    // Represent each node by a circle, with different fill depending on whether
	    // it has any hidden chidlren
	    nodeEnter.append("circle")
		.attr("r", 1e-6)
		.style("stroke", "steelblue")
		.style("stroke-width", 1.5)
		.style("fill", function(d) { return d._branchset ? "lightsteelblue" : "#fff"; });

	    // Label nodes if they are at the current tip of the tree
	    nodeEnter.append("text")
		.text(function(d)  {
		    if (!d.branchset) {
			return d.name;
		    }
		    return (d);
		})
		.style("opacity", 0.0)
		.attr("transform", "rotate(90)");

	    // When collapsing a subtree, calculate where to move the remaining points
	    var nodeUpdate = node.transition()
		.duration(x.duration)
		.attr("transform", function (d) {
		    return "translate(" + canvasScaleX(d.x) + "," + d.y + ")"
		});

	    // When updating the tree, ensure node properties are preserved
	    nodeUpdate.select("circle")
		.attr("r", function(d) { return rScale(1.0 + d.counts[selectValue])})
		.style("fill", function(d) { return d._branchset ? "lightsteelblue" : "#fff"; })

	    // After an update, different nodes (the new tips) will have labels
	    nodeUpdate.select("text")
		.text(function(d)  {
		    if (!d.branchset) {
			return d.name;
		    }
		})
		.style("opacity", 1)
		.attr("transform", "translate(0, 10)rotate(90)");

	    // Store the new positions of all the nodes as 'old positions', for future
	    // updates
	    nodes.forEach(function(d) {
		d.x0 = d.x;
		d.y0 = d.y;
	    });

	    // Calculate where to move tips when they are being hidden
	    var nodeExit = node.exit().transition()
		.duration(x.duration)
		.attr("transform", function (d) {
		    return "translate(" + canvasScaleX(source.x) + "," + source.y + ")"
		})
		.remove();

	    // Make disappearing nodes become smaller
	    nodeExit.select("circle")
		.attr("r", 1e-06)

	    // Make labels disappear when their nodes disappear
	    nodeExit.select("text")
		.text(function(d)  {
		    if (!d.branchset) {
			return d.name;
		    }
		})
		.style("opacity", 0.0);

	    // Initialize the links, and associate with the node ('links' is defined above)
	    var link = instance.vis.selectAll("path.link")
		.data(links,  function(d) { return d.target.id; });

	    // Draw a path representing the link, when we are adding it to the screen
	    link.enter().insert("path", "g")
		.attr("class", "link")
		.attr("d", step)
		.style("stroke-opacity", 0.0);

	    // Link properties after it has been fully loaded (after an update)
	    link.transition()
		.duration(x.duration)
		.attr("d", step)
		.style("stroke", function(d) { return edgeColorScale(d.target.boot);})
		.style("stroke-opacity", 1.0)
		.style("stroke-width", function(d) {return edgeWidthScale(d.target.boot);})

	    // Specify how a node disappears when we collapse the tree
	    link.exit().transition()
		.duration(x.duration)
		.style("stroke-opacity", 0.0)
		.remove()
	}

	// Recursive updating down all the visible children
	function click(d) {
	    if (d.branchset) {
		d._branchset = d.branchset;
		d.branchset = null;
	    } else {
		d.branchset = d._branchset;
		d._branchset = null;
	    }
	    update(d);
	}

	// Function to describe visualization update upon changing the dropdown element
	selectValue=opts[0];
	function onchange() {
	    // What value is currently selected by the dropdown?
	    selectValue = d3.select("select").property("value");

	    // Extract all the node information
	    var nodes = instance.cluster.nodes(tree);
	    phylo(nodes[0], 0);
	    nodes.reverse();

	    // Enter our data into the ndoes
	    var node = instance.vis.selectAll("g.node")
		.data(nodes.filter(function(n) { return n.x !== undefined; }), function(d) { return d.id || (d.id = ++i); });

	    // Draw the nodes in the correct positions
	    var nodeUpdate = node.transition()
		.duration(x.duration)
		.attr("transform", function (d) {
		    return "translate(" + canvasScaleX(d.x) + "," + d.y + ")"
		});

	    // Update the size of the circles based on the new dropdown selection
	    nodeUpdate.select("circle")
		.attr("r", function(d) {
		    return rScale(1.0 + d.counts[selectValue])})
		.style("fill", function(d) { return d._branchset ? "lightsteelblue" : "#fff"; });
	};

	// Start the click updating function
	x.x0=instance.width / 2;
	x.y0=0;
	update(x);
    }
});
