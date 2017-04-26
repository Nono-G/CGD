//var couleurs2 =BLANC; NOIR; VERT; BLEU; ROUGE;
//               JAUNE; ORANGE; CYAN; VIOLET; FUCHSIA;
//               MARRON; SAPIN; BORDEAUX; MARINE; PRUNE;
//				 GRIS;
var couleurs2 = ["#ffffff", "#000000", "#00cc00", "#0000ff", "#cc0000",
				 "#ffff00", "#ff6600", "#00ffff", "#660066", "#ff00ff",
				 "#663300", "#003300", "#660000", "#00004d", "#330033",
				 "4d4d4d"];
//var ticks=0;
function layout(){
	var svg = d3.select("svg");
	var lines = svg.selectAll("line")
	var circles = svg.selectAll("circle");
	var nodes = toNodeArray(circles[0]);
	var links = toEdgeArray(lines[0], nodes);
	//console.log(lines);
	//console.log(circles);
	//console.log(nodes);
	//console.log(links);
	circles.data(nodes);
	lines.data(links);
	force = d3.layout.force()
		.charge(document.getElementById("vcinput").value)
		.linkDistance(document.getElementById("elinput").value)
		.size([1600, 900])
		.alpha(0.001)
		.gravity(document.getElementById("gvinput").value)
		.nodes(nodes)
		.links(links)
		.start()
		;
	//
	circles.style("fill", function(d){return couleurs2[d.state%couleurs2.length]})
		.attr("r", document.getElementById("vrinput").value)
		.call(force.drag);

	lines.style("stroke-width", document.getElementById("ewinput").value);
	//
	force.on("tick", function() {
		lines.attr("x1", function(d) { return d.source.x; })
			.attr("y1", function(d) { return d.source.y; })
			.attr("x2", function(d) { return d.target.x; })
			.attr("y2", function(d) { return d.target.y; })
			.attr("sourceID",function(d) {return d.source.name;})
			.attr("targetID",function(d) {return d.target.name;});

		circles.attr("cx", function(d) { return d.x; })
			.attr("cy", function(d) { return d.y; });
		//ticks++;
		//if(ticks > 1){
			fillPosInputs();
		//	ticks=0;
		//}
	});

	force.on("end", function(){
		fillPosInputs();
	});
}

function fillPosInputs(){
	var circles = document.getElementsByClassName("node");
	var i = 0;
	var tx = "";
	while(i < circles.length){
		var name = circles[i].getAttribute("name");
		var cx = circles[i].getAttribute("cx");
		cx = cx.substring(0, cx.indexOf("."));
		var cy = circles[i].getAttribute("cy");
		cy = cy.substring(0, cy.indexOf("."));
		tx = tx +name+";"+cx+";"+cy+"|";
		i++;
	}
	document.getElementById("pos_input_p").value=tx;
	document.getElementById("pos_input_n").value=tx;
}

function toNodeArray(circles){
	var nodeArray = [];
	var i = 0;
	while(i < circles.length){
		nodeArray.push({
			name:circles[i].getAttribute("name"),
			state:circles[i].getAttribute("state"),
			x:parseInt(circles[i].getAttribute("cx")),
			y:parseInt(circles[i].getAttribute("cy"))
		});
		//console.log(i+" : "+nodeArray[i].name);
		i++;
	}
	return nodeArray;
}

function toEdgeArray(lines, nodes){
	var edgeArray = []
	var i = 0;
	while(i < lines.length){
		var x = 0;
		while(x < nodes.length && nodes[x].name != lines[i].getAttribute("src")){x++;}
		var k = 0;
		while(k < nodes.length && nodes[k].name != lines[i].getAttribute("dst")){k++;}
		//console.log(i+" : "+x+" : "+k+" > "+lines[i].getAttribute("src")+" : "+lines[i].getAttribute("dst")+" = "+nodes[x].name+" : "+nodes[k].name);
		//console.log(x);
		//console.log(k);
		//console.log(x+" : "+lines[x].getAttribute("src"));
		//console.log(k+" : "+lines[k].getAttribute("dst"));
		//var l = 0;
		//while( l < edgeArray.length &&
		// 	 (edgeArray[l][source]!=x || edgeArray[l][target]!=k) &&
		// 	 (edgeArray[l][source]!=k || edgeArray[l][target]!=x)){l++;}
		//if(l==edgeArray.length){
			edgeArray.push({
				source:x,
				target:k
			});
			//}
		i++;
	}
	//console.log("NONO");
	//console.log(edgeArray);
	return edgeArray;
}

function updatePostNumbers(){
	document.getElementById("vr_input_p").value = document.getElementById("vrinput").value;
	document.getElementById("vr_input_n").value = document.getElementById("vrinput").value;
	document.getElementById("vc_input_p").value = document.getElementById("vcinput").value;
	document.getElementById("vc_input_n").value = document.getElementById("vcinput").value;
	document.getElementById("el_input_p").value = document.getElementById("elinput").value;
	document.getElementById("el_input_n").value = document.getElementById("elinput").value;
	document.getElementById("ew_input_p").value = document.getElementById("ewinput").value;
	document.getElementById("ew_input_n").value = document.getElementById("ewinput").value;
	document.getElementById("ad_input_p").value = document.getElementById("adinput").value;
	document.getElementById("ad_input_n").value = document.getElementById("adinput").value;
	document.getElementById("gv_input_p").value = document.getElementById("gvinput").value;
	document.getElementById("gv_input_n").value = document.getElementById("gvinput").value;
	//console.log(document.getElementById("adinput").value);
}

function toAutoMode0(){
	document.getElementById("automode_input_p").value = 0;
	document.getElementById("automode_input_n").value = 0;
}
function toAutoMode1(){
	var amdelay = document.getElementById("adinput").value;
	document.getElementById("automode_input_p").value = 1;
	document.getElementById("automode_input_n").value = 1;
	window.setTimeout(function(){autoTransition()}, amdelay);
}
function toAutoMode2(){
	var amdelay = document.getElementById("adinput").value;
	document.getElementById("automode_input_p").value = 2;
	document.getElementById("automode_input_n").value = 2;
	window.setTimeout(function(){autoTransition()}, amdelay);
}

function autoTransition(){
	var automode = document.getElementById("automode_input_n").value;
	if(automode==1){
		document.next.submit();
	}else if(automode==2){
		document.previous.submit();
	}
}
