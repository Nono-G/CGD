
var width = 960,
    height = 500;


var color = d3.scale.category20();

var force = d3.layout.force()
    .charge(-120)
    .linkDistance(30)
    .size([width, height]);
    
  var svg=null;  

function Init(){
	 svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height);  
}


function displayJSON(file){
	 var fr = new FileReader();

  fr.onload = function(e) { 

    var data = JSON.parse(e.target.result)
 	  console.log(data)
  	display(data)

   }

	fr.readAsText(file)
	
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

  var link = svg.selectAll(".link")
      .data(graph.links)
      .enter().append("line")
      .attr("class", "link")
      .style("stroke-width", function(d) { return Math.sqrt(d.value); });

  var node = svg.selectAll(".node")
      .data(graph.nodes)
      .enter().append("circle")
      .attr("class", "node")
      .attr("r", 5)
      .attr("name",function(d){return d.name})
			.style("fill", function(d) { return color(d.state); })
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





