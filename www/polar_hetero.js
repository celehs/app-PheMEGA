function setObservation(objectId,markNum){
  if(domDoExist(objectId)){
    setObservationOp(objectId,markNum)
  }
}

function domDoExist(objectId){
  if(document.getElementById(objectId)){
    if(document.getElementById(objectId).getElementsByTagName("rect")[0]){
      return true;
    }
  }
  return false;
}

function setObservationOp(objectId,markNum){
  // Appear the first time: draw a circle
  redraw(objectId,markNum);
  // set a listener: redraw on resize
  let observingDom = document.getElementById(objectId).getElementsByTagName("rect")[0];
  let observe = new MutationObserver(function (mu, ob) {
    // console.log("DOM updated");
    redraw(objectId,markNum);
  })
  observe.observe(observingDom, { attributes: true, childList: true, subtree: true });
}

// Store all insertedDoms
var insertedDom = new Map();
function redraw(objectId,markNum){
  // Get dom instances
  let domInstance = document.getElementById(objectId);
  let pathDoms = domInstance.getElementsByTagName("path");
  let textDoms = domInstance.getElementsByTagName("text");
  let i =0;
  // find the index of specific text dom
  for(key in textDoms){
    let tempDomInnerHTML = textDoms[key].innerHTML;
    if(typeof(tempDomInnerHTML) === "string" && !isNaN(Number(tempDomInnerHTML.replace(/,/g, ''))))
    {
      i++;
      if(Number(tempDomInnerHTML.replace(/,/g, ''))===markNum)
        {break;}
    }
  }
  // insert a dom to draw the line, deleted and insert to redraw if already exist
  let circle = document.createElementNS("http://www.w3.org/2000/svg", 'path'); //Create a path in SVG's namespace
  circle.setAttribute("d","M "+pathDoms[0].getAttribute("d").split("M")[i]); //Set path's data
  circle.style.stroke = "red"; //Set stroke colour
  circle.style.fill = "none"; //Set fill colour
  if(insertedDom.get(objectId)){
    insertedDom.get(objectId).remove();
    insertedDom.delete(objectId);
  }
  insertedDom.set(objectId, domInstance.getElementsByTagName("g")[0].insertBefore(circle,textDoms[0]));
}

setInterval("setObservation('polar_bt',1)", 500)
setInterval("setObservation('polar_qt',0)", 500)