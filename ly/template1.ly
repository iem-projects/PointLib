\version "2.16.2"

\layout {
  ragged-right = ##t

  \context { \Score
    % tuplet handling
    tupletFullLength = ##t
    \override TupletBracket #'bracket-visibility = ##t
    \override TupletBracket #'padding = #2
    % allow tuplet bracket to always be visible, even for short tuplets.
    \override TupletBracket #'springs-and-rods = #ly:spanner::set-spacing-rods
    \override TupletBracket #'minimum-length = #3
	 \override TupletNumber #'text = #tuplet-number::calc-fraction-text

	 \remove Bar_number_engraver
    \override TimeSignature #'X-extent = #'(0 . 3)
    \override InstrumentName #'X-extent = #'(0 . 4)
	 
%%%%%%%%% version 1: proportional %%%%%%%%%
	 
%    proportionalNotationDuration = #(ly:make-moment 1 56)
%    \override SpacingSpanner #'strict-note-spacing = ##t  
%    \override SpacingSpanner #'strict-grace-spacing = ##t
%    \override SpacingSpanner #'uniform-stretching = ##t

%%%%%%%%% version 2: non-proportional %%%%%%%%%

    \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 32)
  }
}

\paper {
  after-title-spacing = #'((space . 0) (padding . 1.5) (stretchability . 3) (minimum-distance . 0))
}

#(set-default-paper-size "a4")
#(set-global-staff-size 16)

