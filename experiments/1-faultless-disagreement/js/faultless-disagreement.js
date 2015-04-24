



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
    present : _.shuffle(stimuli),
    present_handle : function(stim) {
      $(".err").hide();
      // $('input[name="sense"]:checked').attr('checked',false);
      this.stim = stim; //FRED: allows you to access stim in helpers
      //var noun_data = _.sample(corpus.Noun)
      //this.noun_data = noun_data;
      //var noun = noun_data.noun;
      //var animacy = noun_data.animacy;

      this.verbs = _.shuffle(["is","is not"])

      var male_names_list = _.shuffle(male_names);

      var man1 = male_names_list[0];
      var man2 = male_names_list[1];

      var female_names_list = _.shuffle(female_names);

      var woman1 = female_names_list[0];
      var woman2 = female_names_list[1];

      $(".woman").html(woman1)

      $(".man1").html(man1);

      $(".man2").html(man2);

      $(".noun").html(stim.Noun);

      //$(".woman1").html(woman1);

      //$(".woman2").html(man2);

      $(".utterance1").html("\"That "+ stim.Noun + " " + this.verbs[0] + " " + stim.Predicate + ".\"");

      $(".utterance2").html("\"That "+ stim.Noun + " " + this.verbs[1] + " "  + stim.Predicate + ".\"");

      this.sentence_types = _.shuffle(["yes","no"]);
      var sentences = {
        "yes": "Yes, it's a matter of opinion.",
        "no": "No, somebody must be wrong.",
      };

      this.n_sliders = this.sentence_types.length;
      $(".slider_row").remove();
      for (var i=0; i<this.n_sliders; i++) {
        var sentence_type = this.sentence_types[i];
        var sentence = sentences[sentence_type];
        $("#multi_slider_table").append('<tr class="slider_row"><td class="slider_target" id="sentence' + i + '">' + "<font size='4'>" + sentence + "</font>" + '</td><td colspan="2"><div id="slider' + i + '" class="slider">-------[ ]--------</div></td></tr>');
        utils.match_row_height("#multi_slider_table", ".slider_target");
      }

      this.init_sliders(this.sentence_types);
      exp.sliderPost = [];
    },

    button : function() {
      if (exp.sliderPost.length < this.n_sliders) {
        $(".err").show();
      } else {
        this.log_responses();
        _stream.apply(this); //use _stream.apply(this); if and only if there is "present" data.
      }
    },

    init_sliders : function(sentence_types) {
      for (var i=0; i<sentence_types.length; i++) {
        var sentence_type = sentence_types[i];
        utils.make_slider("#slider" + i, this.make_slider_callback(i));
      }
    },
    make_slider_callback : function(i) {
      return function(event, ui) {
        exp.sliderPost[i] = ui.value;
      };
    },
    log_responses : function() {
      for (var i=0; i<this.sentence_types.length; i++) {
        var sentence_type = this.sentence_types[i];
        exp.data_trials.push({
          "faultless" : sentence_type,
          "response" : exp.sliderPost[i],
          "noun" : this.stim.Noun,
          "predicate" : this.stim.Predicate,
          "sentence" : this.stim.Sentence,
          // "sense" : $('input[name="sense"]:checked').val(),        
          "slide_number" : exp.phase
        });
      }
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