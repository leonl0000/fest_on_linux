;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;
;;;                Centre for Speech Technology Research                  ;;
;;;                     University of Edinburgh, UK                       ;;
;;;                       Copyright (c) 1996,1997                         ;;
;;;                        All Rights Reserved.                           ;;
;;;                                                                       ;;
;;;  Permission is hereby granted, free of charge, to use and distribute  ;;
;;;  this software and its documentation without restriction, including   ;;
;;;  without limitation the rights to use, copy, modify, merge, publish,  ;;
;;;  distribute, sublicense, and/or sell copies of this work, and to      ;;
;;;  permit persons to whom this work is furnished to do so, subject to   ;;
;;;  the following conditions:                                            ;;
;;;   1. The code must retain the above copyright notice, this list of    ;;
;;;      conditions and the following disclaimer.                         ;;
;;;   2. Any modifications must be clearly marked as such.                ;;
;;;   3. Original authors' names are not deleted.                         ;;
;;;   4. The authors' names are not used to endorse or promote products   ;;
;;;      derived from this software without specific prior written        ;;
;;;      permission.                                                      ;;
;;;                                                                       ;;
;;;  THE UNIVERSITY OF EDINBURGH AND THE CONTRIBUTORS TO THIS WORK        ;;
;;;  DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING      ;;
;;;  ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT   ;;
;;;  SHALL THE UNIVERSITY OF EDINBURGH NOR THE CONTRIBUTORS BE LIABLE     ;;
;;;  FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES    ;;
;;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN   ;;
;;;  AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,          ;;
;;;  ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF       ;;
;;;  THIS SOFTWARE.                                                       ;;
;;;                                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Set up Donovan diphones
;;;
;;;  Note in its current setting this voice requires OALD which is 
;;;  free only for non-commercial use
;;;

(defvar donovan_diphone_dir (cdr (assoc 'don_diphone voice-locations)))

(require 'mrpa_phones)
(require 'holmes_phones)  ;; diphones are in Holmes, but we map from mrpa
(require 'pos)
(require 'phrase)
(require 'tobi)
(require 'f2bf0lr)
(require 'mrpa_durs)   ;; average durations for mrpa phones
(require 'gswdurtreeZ)
(require_module 'donovan)

(setup_oald_lex)

(define (setup_don_diphone)
  (Donovan_Init
   (list
    (list 'index_file (string-append donovan_diphone_dir "diphlocs.txt"))
    (list 'diphone_file (string-append donovan_diphone_dir "lpcdiphs.bin"))
    )))

(define (don_postlex_syllabics utt)
"(don_postlex_syllabics utt)
Becuase the lexicon is somewhat random in its used of syllable l n and
m this is designed to post process the output inserting schwa before
them.  Ideally the lexicon should be fixed."
  (mapcar
   (lambda (s)
     (if (and (member_string (item.name s) '("l" "n" "m"))
	      (string-equal "coda" (item.feat s "seg_onsetcoda"))
	      (not (member_string (item.feat s "p.name") '(l r)))
	      (string-equal "-" (item.feat s "p.ph_vc")))
	 (item.relation.insert 
	  s 'SylStructure
	  (item.insert s (list "@") 'before)
	  'before)))
   (utt.relation.items utt 'Segment)))

(define (voice_don_diphone)
"(voice_don_diphone)
  Set up the current speaker to be British male RP speaker, LPC Donovan 
  diphones."
  (voice_reset)
  (Parameter.set 'Language 'britishenglish)
  ;; Phone set
  (PhoneSet.select 'mrpa)
  (Parameter.set 'PhoneSet 'mrpa)
  ;; Tokenization rules
  (set! token_to_words english_token_to_words)
  ;; POS tagger
  (set! pos_lex_name "english_poslex")
  (set! pos_ngram_name 'english_pos_ngram)
  (set! pos_supported t)
  (set! guess_pos english_guess_pos)   ;; need this for accents
  ;; Lexicon selection
  (lex.select "oald")
  (set! postlex_rules_hooks (list postlex_apos_s_check
				  don_postlex_syllabics))
  ;; Phrase prediction
  (Parameter.set 'Phrase_Method 'prob_models)
  (set! phr_break_params english_phr_break_params)
  ;; Accent and tone prediction
  (set! int_tone_cart_tree f2b_int_tone_cart_tree)
  (set! int_accent_cart_tree f2b_int_accent_cart_tree)
  ;; F0 prediction
  (set! f0_lr_start f2b_f0_lr_start)
  (set! f0_lr_mid f2b_f0_lr_mid)
  (set! f0_lr_end f2b_f0_lr_end)
  (Parameter.set 'Int_Method Intonation_Tree)
  (set! int_lr_params
	'((target_f0_mean 135) (target_f0_std 30)
	  (model_f0_mean 170) (model_f0_std 34)))
  (Parameter.set 'Int_Target_Method Int_Targets_LR)
  ;; Duration prediction
  (set! duration_cart_tree gsw_duration_cart_tree);
  (set! duration_ph_info gsw_durs)
  (Parameter.set 'Duration_Method Duration_Tree_ZScores)
  (Parameter.set 'Duration_Stretch 1.0)
  ;; Waveform synthesizer: Donovan LPC diphones
  (Parameter.set 'Synth_Method Donovan_Synthesize)
  (setup_don_diphone)

  (set! after_synth_hooks 
	(lambda (utt) 
	  (utt.wave.rescale utt 1.6)))

  (set! current-voice 'don_diphone)
)

(proclaim_voice
 'don_diphone
 '((language english)
   (gender male)
   (dialect british)
   (description
    "This voice uses a very small and efficient pulse-excited LPC
     diphone synthesis method.  It was originally written by Steve
     Isard.  The front end uses the same British English lexicon,
     intonation and duration methods as rab_diphone.")))

(provide 'don_diphone)
