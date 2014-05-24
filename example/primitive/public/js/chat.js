(function(){
    webix.ui({
        rows:[
            getViewHeader(),
            { cols:[
                 { template:"left col" },
                 { width:600, rows:[
                       { view:"layout", container:"scroll", type:"line",
                           rows:[
                               { template:"<span style='font-size:16px; text-align:center; font-weight:bold'>John Keats\' Verses</span>", type:"header"},
                               { view:"scrollview", id:"verses", height:300, scroll:"y",
                                   body:{
                                       rows:[
                                           { id:"verse_1", template:"html->my_box1", autoheight:true },
                                           { template:"<i>Scroll down for the next verse</i>", height:30 },
                                           { id:"verse_2", template:"html->my_box2", autoheight:true },
                                           { template:"<i>Scroll down for the next verse</i>", height:30 },
                                           { id:"verse_3", template:"html->my_box3", autoheight:true },
                                           { template:"<i>Scroll down for the next verse</i>", height:30 },
                                           { id:"verse_4", template:"html->my_box4", autoheight:true }
                                       ]
                                   } //end of scrollview body
                               },
                           ]
                       }]
                 },
                 { template: "right col" }
             ]}
        ]
    });
})();