/*This script takes a TIM video as input, partitions it into 12 subclips, 
    and saves them in a subject-specific psychopy experiment folder.*/

app.project.close(CloseOptions.DO_NOT_SAVE_CHANGES);
app.newProject();

//create a new FolderItem in project, with name "comps"
var compFolder = app.project.items.addFolder("TIM_SOS");

//setting up
var tag_id = prompt("Participant's TAG ID #:", "999");
var wave = "sos";
//var wave = prompt("Data Collection Wave:", "sos");
var start_time = prompt("Choose a start time:", "3.00000");
var save_folder = "I:\\dsnlab\\TAG\\behavior\\SOS\\clips\\TAG" + tag_id + "\\";
//var save_folder = prompt("Where should the exported clips be saved?", "I:\\dsnlab\\TAG\\behavior\\SOS\\clips\\TAG" + tag_id + "\\");

//var play_folder = prompt("Where should should PsychoPy3 look for the stimuli?", "C:\\Users\\mbyrne\\Documents\\SOS\\TIM_Scanner\\subj\\");

app.project.importFile(new ImportOptions(File("I:\\dsnlab\\TAG\\behavior\\This_Is_Me\\SOS\\TAG" + tag_id + "W" + wave + ".mov")));

var start_vid = Number(start_time);

var onset_duration = new Array(start_vid, start_vid + 17.52154, 
                                                 start_vid + 17.52154, start_vid + 32.19503, 
                                                 start_vid + 32.19503, start_vid + 51.69913, 
                                                 start_vid + 51.69913, start_vid + 66.02796, 
                                                 start_vid + 66.02796, start_vid + 78.05277, 
                                                 start_vid + 78.05277, start_vid + 95.66065, 
                                                 start_vid + 95.66065, start_vid + 118.24431, 
                                                 start_vid + 118.24431, start_vid + 135.92152, 
                                                 start_vid + 135.92152, start_vid + 148.80235, 
                                                 start_vid + 148.80235, start_vid + 168.59260, 
                                                 start_vid + 168.59260, start_vid + 183.87435, 
                                                 start_vid + 183.87435, start_vid + 193.97597);

var dur = app.project.item(1).duration;
var fps = app.project.item(1).frameRate;
var width = app.project.item(1).width;
var height = app.project.item(1).height;

var clip = 1;
while(clip<13){
    app.project.items.addComp("block"+clip, width, height, 1, dur, fps);
    clip++;
}

var tim = app.project.item(13);

var activeComp = 1;
while(activeComp<13){
    var comp = app.project.item(activeComp);
    var layerCollection = comp.layers;
    layerCollection.add(tim);
    activeComp++;
}

var on_dur_new = new Array();
var i=0;
while(i<24){
    on_dur_new[i] = timeToCurrentFormat(onset_duration[i], Number(fps));
    i++;
}

var i=0;
var j=1;
while(i<24 && j<13){
    app.project.item(j).layer(1).inPoint = onset_duration[i];
    i++;
    app.project.item(j).layer(1).outPoint = onset_duration[i];
    i++;
    j++;
}

var trim = 1;
while(trim<13){
    
    app.project.item(trim).layer(1).startTime = 
    -app.project.item(trim).layer(1).inPoint;
    
    app.project.item(trim).duration = 
    app.project.item(trim).layer(1).outPoint;
    
    app.project.item(trim).preserveNestedFrameRate = true;
    
    trim++;
}

i = 1;
while(i<13){
    var resultFile_backup = new File(save_folder + app.project.item(i).name);
    //var resultFile_stimuli = new File(play_folder + app.project.item(i).name);
    var renderQueue = app.project.renderQueue;
    var render = renderQueue.items.add(app.project.item(i));
    render.outputModules[1].file = resultFile_backup;
    //var render2 = renderQueue.items.add(app.project.item(i));
    //render2.outputModules[1].file = resultFile_stimuli;
    i++;
}

app.project.renderQueue.queueInAME(true);
