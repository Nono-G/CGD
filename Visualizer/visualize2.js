
var width = 1250,
    height = 750;


var color = d3.scale.category20();
var nonocouleurs = ["#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477", "#66aa00", "#b82e2e", "#316395", "#994499", "#22aa99", "#aaaa11", "#6633cc", "#e67300", "#8b0707", "#651067", "#329262", "#5574a6", "#3b3eac"];
//var couleurs2 =BLANC; NOIR; VERT; BLEU; ROUGE; JAUNE; ORANGE; CYAN
var couleurs2 = ["#ffffff", "#000000","#00cc00","#0000ff","#cc0000","#ffff00","#ff6600","#00ffff"];
var force=null;
    
  var svg=null;  

function Init(){
	 svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);  
}

var file_triche=null;

function displayJSON(file){
  file_triche = file;
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
  if(file_triche != null){
    displayJSON(file_triche);
  }
}


var currentGraph=null;
var randomSHIAT=
{
  "nodes":[
    {"name":"ONE","group":1},
    {"name":"TWO","group":1},
     {"name":"THREE","group":1},
      {"name":"FOUR","group":1}
   
  ],
  "links":[
    {"source":1,"target":0,"value":1},
     {"source":2,"target":0,"value":1},
      {"source":3,"target":0,"value":1}
  ]
};
function DisplayRAW(graph){
  force
      .nodes(graph.nodes)
      .links(graph.links)
      .start();

  var link = svg.selectAll(".links")
      .data(graph.links)
      .enter().append("line")
      .attr("class", "links")
      .style("stroke-width", function(d) { return Math.sqrt(d.value); });

  var node = svg.selectAll(".node")
      .data(graph.nodes)
      .enter().append("circle")
      .attr("class", "node")
      .attr("r", 5)
      .attr("name",function(d){return d.name})
  	  .style("fill","black")
      .call(force.drag);
	
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


function display(graph){
  console.log(graph);
//   graph={
//   "nodes":[
//     {"name":"ONE","group":1},
//     {"name":"TWO","group":1},
//      {"name":"THREE","group":1},
//       {"name":"FOUR","group":1}
   
//   ],
//   "links":[
//     {"source":1,"target":0,"value":1},
//      {"source":2,"target":0,"value":1},
//       {"source":3,"target":0,"value":1}
//   ]
// };
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
      .attr("class", "link-g")
      ;

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
        .style("fill", function(d){return couleurs2[d.state%couleurs2.length]})
        /*.call(force.drag)*/;

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
var circles;
function cleanDIFF(graph){

 svg.selectAll("circle").filter(function(cir){
      for(j=0; j< graph.nodes.length; j++){
        if(cir.name == graph.nodes[j].name){
           return false;
        }
      }
      return true;
   }).remove();
 
 svg.selectAll("line").filter(function(lin){
      for(j=0; j< graph.nodes.length; j++){
      for(i=0; i< graph.nodes.length; i++){
        if((lin.sourceID == graph.nodes[j].name) & (lin.targetID == graph.nodes[i].name)){
           return false;
        }
      }
      }
      return true;
   }).remove();
  display(graph);
}


// function DisplayDIFF(graph){

// 	var nodeBEFORE=force.nodes();
// 	var nodeAFTER=graph.nodes;
//   var nodesDEF=[];

// 	// function removeDiff(node){
// 	// 		var tm=false;
// 	// 		for (node2 of nodeAFTER){
// 	// 			if(node2.name == node.id){tm=true}
			
// 	// 		}
// 	// 		if(!tm){
// 	// 			svg.remove(node);
// 	// 			console.log("Removing "+node.name);
// 	// 		}else{
// 	// 		console.log("Not removing "+node.name);}
// 	// }
// 	// for (node of nodeBEFORE){
// 	// 	removeDiff(node);
// 	// }


//   for (i = 0; i < nodeAFTER.length; i++) {
//       nodesDEF.push(nodeAFTER[i]);
//   }
//   force
//       .nodes(nodesDEF)
//       .links(graph.links)
//       .start();

  
//   console.log(nodesDEF);

// }





