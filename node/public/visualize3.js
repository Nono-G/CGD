//var couleurs2 =BLANC; NOIR; VERT; BLEU; ROUGE;
//               JAUNE; ORANGE; CYAN; VIOLET; FUCHSIA;
//               MARRON; SAPIN; BORDEAUX; MARINE; PRUNE;
var couleurs2 = ["#ffffff", "#000000", "#00cc00", "#0000ff", "#cc0000",
				 "#ffff00", "#ff6600", "#00ffff", "#660066", "#ff00ff",
				 "#663300", "#003300", "#660000", "#00004d", "#330033"];
var ticks=0;
function layout(){
	var svg = d3.select("svg");
	var lines = svg.selectAll("line")
	var circles = svg.selectAll("circle");
	var nodes = toNodeArray(circles[0]);
	var links = toEdgeArray(lines[0], nodes);
	circles.data(nodes);
	lines.data(links);
	force = d3.layout.force()
		.charge(document.getElementById("vcinput").value)
		.linkDistance(document.getElementById("elinput").value)
		.size([1600, 850])
		.alpha(0.001)
		.gravity(0.01)
		.nodes(nodes)
		.links(links)
		.start();
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
		ticks++;
		if(ticks > 1){
			fillPosInputs();
			ticks=0;
		}
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
		nodeArray.push(
			{name:circles[i].getAttribute("name"),
			 state:circles[i].getAttribute("state"),
			 x:parseInt(circles[i].getAttribute("cx")),
			 y:parseInt(circles[i].getAttribute("cy"))
			}
		);
		i++;
	}
	return nodeArray;
}

function toEdgeArray(lines, nodes){
	var linksArray = []
	var i = 0;
	while(i < lines.length){
		var x = 0;
		while(x < lines.length && nodes[x].name != lines[i].getAttribute("src")){x++;}
		var k = 0;
		while(k < lines.length && nodes[k].name != lines[i].getAttribute("dst")){k++;}
		//var l = 0;
		//while( l < linksArray.length &&
		// 	 (linksArray[l][source]!=x || linksArray[l][target]!=k) &&
		// 	 (linksArray[l][source]!=k || linksArray[l][target]!=x)){l++;}
		//if(l==linksArray.length){
			linksArray.push({source:x,target:k});
			//}
		i++;
	}
	return linksArray;
}

function updatePostNumbers(){
	document.getElementById("vr_input_p").value = document.getElementById("vrinput").value;
	document.getElementById("vr_input_n").value = document.getElementById("vrinput").value;
	console.log("nono :");
	document.getElementById("vc_input_p").value = document.getElementById("vcinput").value;
	document.getElementById("vc_input_n").value = document.getElementById("vcinput").value;
	document.getElementById("el_input_p").value = document.getElementById("elinput").value;
	document.getElementById("el_input_n").value = document.getElementById("elinput").value;
	document.getElementById("ew_input_p").value = document.getElementById("ewinput").value;
	document.getElementById("ew_input_n").value = document.getElementById("ewinput").value;
	document.getElementById("ad_input_p").value = document.getElementById("adinput").value;
	document.getElementById("ad_input_n").value = document.getElementById("adinput").value;
	console.log(document.getElementById("adinput").value);
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
