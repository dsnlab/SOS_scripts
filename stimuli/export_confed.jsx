/*This script takes a TIM confederate video as input, partitions it into 6 subclips, 
    and saves them in a confederate-specific psychopy experiment folder.*/

app.project.close(CloseOptions.DO_NOT_SAVE_CHANGES);
app.newProject();

//create a new FolderItem in project, with name "comps"
var compFolder = app.project.items.addFolder("TIM_SOS");

var tag_id = prompt("Participant's TAG ID #:", "999");
var wave = prompt("Data Collection Wave:", "sos");
var start_time = prompt("Choose a start time:", "3.00000");
var save_folder = prompt("Where should the exported clips be saved?", "C:\\Users\\mbyrne\\Documents\\SOS\\TIM_Scanner\\confed\\");

app.project.importFile(new ImportOptions(File("C:\\Users\\mbyrne\\Documents\\SOS\\Stimuli\\Videos\\TAG" + tag_id + "W" + wave + ".mp4")));

var start_vid = Number(start_time);

var onset_duration = new Array(start_vid, start_vid + 16.62983, 
                                                 start_vid + 34.15137, start_vid + 52.48223, 
                                                 start_vid + 52.48223, start_vid + 67.64845, 
                                                 start_vid + 85.25633, start_vid + 98.28709, 
                                                 start_vid + 115.96430, start_vid + 127.48277, 
                                                 start_vid + 137.58439, start_vid + 159.17247);

var dur = app.project.item(1).duration;
var fps = app.project.item(1).frameRate;
var width = app.project.item(1).width;
var height = app.project.item(1).height;

var clip = 1;
while(clip<7){
    app.project.items.addComp("block"+clip, width, height, 1, dur, fps);
    clip++;
}

var tim = app.project.item(7);

var activeComp = 1;
while(activeComp<7){
    var comp = app.project.item(activeComp);
    var layerCollection = comp.layers;
    layerCollection.add(tim);
    activeComp++;
}

var on_dur_new = new Array();
var i=0;
while(i<12){
    on_dur_new[i] = timeToCurrentFormat(onset_duration[i], Number(fps));
    i++;
}

var i=0;
var j=1;
while(i<12 && j<7){
    app.project.item(j).layer(1).inPoint = onset_duration[i];
    i++;
    app.project.item(j).layer(1).outPoint = onset_duration[i];
    i++;
    j++;
}

var trim = 1;
while(trim<7){
    
    app.project.item(trim).layer(1).startTime = 
    -app.project.item(trim).layer(1).inPoint;
    
    app.project.item(trim).duration = 
    app.project.item(trim).layer(1).outPoint;
    
    app.project.item(trim).preserveNestedFrameRate = true;
    
    trim++;
}

i = 1;
while(i<7){
    var resultFile = new File(save_folder + app.project.item(i).name);
    var renderQueue = app.project.renderQueue;
    var render = renderQueue.items.add(app.project.item(i));
    render.outputModules[1].file = resultFile;
    i++;
}

app.project.renderQueue.queueInAME(true);
