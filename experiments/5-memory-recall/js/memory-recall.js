



function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions1 = slide({
    name : "instructions1",
    start: function() {
      $(".instruction_condition").html("Between subject intruction manipulation: "+ exp.instruction);
    }, 
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.targets = slide({
    name : "targets",
    start : function() {

      targets = _.filter(stimuli,function(item){ if (item.Encountered=="yes") {return item}; })
      this.sentences = [ ];
      for (var i=0; i<targets.length; i++) {
        this.sentences[i] = targets[i].Predicate1 + " " + targets[i].Predicate2 + " " + targets[i].Noun;
      };
 
      this.n_sliders = this.sentences.length;

      for (var i=0; i<this.n_sliders; i++) {
        var sentence = this.sentences[i];
        $("#target_list").append('<tr class="table_row" align = "center"><td class="target" id="sentence' + i + '">' + sentence + '</td>')
        utils.match_row_height("#target_list", ".table");
      }


    },

    button : function() {
        this.log_responses();
        exp.go();
    },

    log_responses : function() {
        exp.data_trials.push({
          "stimuli" : stimuli
        });
    },
  });

  slides.recall = slide({
    name : "recall",
    present : _.shuffle(stimuli),
    present_handle : function(stim) {
      $(".err").hide();
      $('input[name="test"]:checked').attr('checked',false);
      this.stim = stim;
      pred_num = _.sample([1,2]);

      if (pred_num==1) {
        test_sentence = stim.Predicate1 + " " + stim.Noun;
      } else {
        test_sentence = stim.Predicate2 + " " + stim.Noun;
      }
      

      $("#recall_target").html(test_sentence);

    },

    button : function() {
        this.log_responses();
        _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
      },

    button : function() {
      var ok_to_go_on = true;
      if ($("input[type=radio]:checked").length == 0) {
        ok_to_go_on = false;
      }
      if (ok_to_go_on) {
        this.log_responses();
        _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.        
      } else {
        $(".err").show();
      }
    },  

    log_responses : function() {
        exp.data_trials.push({
          "recall_target" : test_sentence,
          "encountered" : this.stim.Encountered,
          "predicate1" : this.stim.Predicate1,
          "class1" : this.stim.Class1,
          "predicate2" : this.stim.Predicate2,
          "class2" : this.stim.Class2,
          "noun" : this.stim.Noun,
          "noun_class" : this.stim.NounClass,
          "predicate_number" : pred_num,
          "response" : $('input[name="sense"]:checked').val()
        });
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          //"condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.trials = [];
  exp.catch_trials = [];
  exp.instruction = _.sample(["instruction1","instruction2"]);
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions1",'targets','recall', 'subj_info', 'thanks'];
  
  exp.data_trials = [];
  // exp.stimuli = stimuli;
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}