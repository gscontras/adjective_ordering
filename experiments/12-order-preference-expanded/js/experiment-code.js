



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

  slides.multi_slider = slide({
    name : "multi_slider",
    present : _.sample(stimuli,30),
    present_handle : function(stim) {
      $(".err").hide();
      $(".err2").hide();
      this.init_sliders();   
      $('input[name="sense"]:checked').attr('checked',false);   
      exp.sliderPost = null;
      // $('input[name="sense"]:checked').attr('checked',false);
      this.stim = stim; //FRED: allows you to access stim in helpers
      //var noun_data = _.sample(corpus.Noun)
      //this.noun_data = noun_data;
      //var noun = noun_data.noun;
      //var animacy = noun_data.animacy;

      // this.verbs = _.shuffle(["is","is not"])

      var names_list = _.shuffle(names);

      var man1 = names_list[0];
      var man2 = names_list[1];

      $(".man1").html(man1);

      $(".man2").html(man2);

      $(".noun").html(stim.Noun);

      //$(".woman1").html(woman1);

      //$(".woman2").html(man2);

      $(".low").html("\"the "+ stim.Predicate2 + " " + stim.Predicate1 + " " + stim.Noun + "\"");

      $(".high").html("\"the "+ stim.Predicate1 + " " + stim.Predicate2 + " " + stim.Noun + "\"");

		this.n_sliders = 1;

    },

    button : function() {
    	console.log(exp.sliderPost);
      if (exp.sliderPost != null) {
        this.log_responses();
        _stream.apply(this); //use exp.go() if and only if there is no "present" data.
      } else {
      if ($("input[type=radio]:checked").length != 0) {
        $(".err").show();
      } else {
        $(".err").hide();
        $(".err2").show();
      }
    }
    },

    init_sliders : function() {
      utils.make_slider("#slider0", function(event, ui) {
        exp.sliderPost = ui.value;
      });
    },

    log_responses : function() {
        exp.data_trials.push({
          "response" : exp.sliderPost,
          "noun" : this.stim.Noun,  
          "nounclass" : this.stim.NounClass,        
          "predicate1" : this.stim.Predicate1,
          "predicate2" : this.stim.Predicate2,
          "class1" : this.stim.Class1,
          "class2" : this.stim.Class2,                     
          "slide_number" : exp.phase,
          "sense" : $('input[name="sense"]:checked').val()
        });
    },
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
  repeatWorker = false;
  (function(){
      var ut_id = "order-preference-expanded-2";
      if (UTWorkerLimitReached(ut_id)) {
        $('.slide').empty();
        repeatWorker = true;
        alert("You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT' to avoid any impact on your approval rating.");
      }
  })();

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
  exp.structure=["i0", "instructions1",'multi_slider', 'subj_info', 'thanks'];
  
  exp.data_trials = [];
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