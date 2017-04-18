
var width = 1600;
var height = 850;
//var couleurs2 =BLANC; NOIR; VERT; BLEU; ROUGE;
//               JAUNE; ORANGE; CYAN; VIOLET; FUCHSIA;
//               MARRON; SAPIN; BORDEAUX; MARINE; PRUNE;
var couleurs2 = ["#ffffff", "#000000", "#00cc00", "#0000ff", "#cc0000",
				 "#ffff00", "#ff6600", "#00ffff", "#660066", "#ff00ff",
				 "#663300", "#003300", "#660000", "#00004d", "#330033"];
var force=null;
var svg=null;  

function Init(){
	svg = d3.select("body").append("svg")
		.attr("width", width)
		.attr("height", height);  
}

var file_current=null;

function displayJSON(file){
	file_current = file;
	//var file = document.getElementById("finput").value;
	force = d3.layout.force()
		.charge(document.getElementById("vcinput").value)
		.linkDistance(document.getElementById("elinput").value)
		.size([width, height]);
	var fr = new FileReader();
	fr.onload = function(e) { 
		var data = JSON.parse(e.target.result)
		console.log(data)
		display(data)
	 }
	fr.readAsText(file)
}

function reloadDisplay(){
	if(file_current != null){
		displayJSON(file_current);
	}
}


var currentGraph=null;

function display(graph){
	console.log(graph);
	svg.selectAll("g").remove();
	svg.selectAll("circle").remove();
	svg.selectAll("line").remove();
	currentGraph=graph; 
	force.nodes(graph.nodes);
	console.log(graph.links);
	for(var i=0; i< graph.links.length; i++){
		for(var j=0; j< force.nodes().length;j++){
			console.log("Looking for "+graph.links[i].source+" and "+graph.links[i].target);
			if(graph.links[i].source ==  force.nodes()[j].name){ 
				graph.links[i].source =j;
				console.log("New source "+force.nodes()[j].name + " "+j);
			};
			if(graph.links[i].target ==  force.nodes()[j].name){ 
				graph.links[i].target =j;
				console.log("New target "+force.nodes()[j].name + " "+j);
			};
		}
	}
	force.links(graph.links)
		.start();

	var link_groups = svg.selectAll(".link")
		.data(graph.links)
		.enter().append("g")
		.attr("class", "link-g");
	
	var edge_width = document.getElementById("ewinput").value;
	link_groups.append("line")
		.attr("class", "link")
		.style("stroke-width", edge_width);

	link_groups.append("text")
		.attr("class", "ltext")
		.style("fill", "White")
		.text("EDGE :")
		.attr("x", "20")
		.attr("y", "20");

	var link_texts = svg.selectAll(".ltext")
		.data(graph.links)
		.append("tspan")
		.attr("x", "20")
		.attr("dy","1.2em")
		.text(function(d){
			return ("("+d.source.name+" : "+d.portIn+") - ("+d.target.name+" : "+d.portOut+")");
		});

	var link = svg.selectAll(".link");
	
	var node_groups = svg.selectAll(".node")
		.data(graph.nodes)
		.enter().append("g")
		.attr("class", "node-g")
		.attr("name",function(d){return d.name})
		.call(force.drag)
		.attr("state", function(d){return d.state});
	
	var vertex_radius = document.getElementById("vrinput").value;
	node_groups.append("circle")
		.attr("class", "node")
		.attr("r", vertex_radius)
		.style("fill", function(d){return couleurs2[d.state%couleurs2.length]});

	node_groups.append("text")
		.attr("class", "ntext")
		.style("fill", "White")
		.text("VERTEX :")
		.attr("x", "20")
		.attr("y", "20");

	var node_texts = svg.selectAll(".ntext")
		.data(graph.nodes);

	node_texts.append("tspan")
		.attr("x", "20")
		.attr("dy","1.2em")
		.text(function(d){return "Name : "+d.name});

	node_texts.append("tspan")
		.attr("x", "20")
		.attr("dy", "1.2em")
		.text(function(d){return "State : "+d.state});

	var node=svg.selectAll("circle");

	force.on("tick", function() {
		link.attr("x1", function(d) { return d.source.x; })
			.attr("y1", function(d) { return d.source.y; })
			.attr("x2", function(d) { return d.target.x; })
			.attr("y2", function(d) { return d.target.y; })
			.attr("sourceID",function(d) {return d.source.name;})
			.attr("targetID",function(d) {return d.target.name;});

		node.attr("cx", function(d) { return d.x; })
			.attr("cy", function(d) { return d.y; });

	});
}

function displayPrevious(){
	console.log("A");
}

function displayNext(){
	console.log("B");
}