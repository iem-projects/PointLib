LilyShowableObj.lilyCmd = "~/bin/lilypond".standardizePath;

(
~cell = nil ! 25;

//each is [d, p], where d is total duration in 1/8, and p is an array of relative durations (partitions).
//negative values in the partition array indicate rests (pauses).
~cell[ 0] = [ 2, [1, 1, 1]];               // 1/4
~cell[ 1] = [ 2, [1, 3]];
~cell[ 2] = [ 2, [3, 1]];
~cell[ 3] = [ 2, [1, 1, 1, 1, 1]];
~cell[ 4] = [ 2, [4, 1]];
~cell[ 5] = [ 2, [1, 1, 4]];
~cell[ 6] = [ 2, [-1, 1, 1, -1, 1, 1]];
~cell[ 7] = [ 2, [1, 1, 1, 1, -1, 1, 2]];
~cell[ 8] = [ 2, [-4, 1, 1, 1]];
~cell[ 9] = [ 4, [-1, 2, 1]];              // 1/2
~cell[10] = [ 4, [-1, 3]];
~cell[11] = [ 4, [3, 2, 1]];
~cell[12] = [ 4, [-2, 2, -1, 3]];
~cell[13] = [ 4, [-3, 2, 1, -2]];
~cell[14] = [ 4, [3, 1, -1, 1, -2]];
~cell[15] = [ 4, [6, 1, 1, -2]];
~cell[16] = [ 4, [2, 7, 3]];
~cell[17] = [ 4, [2, 9, 1]];
~cell[18] = [ 4, [1, 1, 7, 1, 1, 5]];
~cell[19] = [ 6, [1, 1, 3, 1, 1, 5]];      // 3/4
~cell[20] = [ 7, [7, 6, 5, 4, 3, 2, 1]];   // 7/8
~cell[21] = [ 8, [1, 2, 2, 5, -2]];        // 1/1
~cell[22] = [ 8, [1, 2, 3, 4, 2, 3, 5, 1, 7, -4]];
~cell[23] = [12, [9, 15]];                 // 6/4
~cell[24] = [14, [12, 5, 11]];             // 7/4
)

// (
// c.do { arg arr, i;
// 	LilyShowableObj.fileName = "~/Desktop/cell" ++ (i+1);
// 	RhythmicCell(arr).plot;
// }
// )

LilyShowableObj.fileName = "~/Desktop/cells";
~cell.collect { |arr| RhythmicCell(arr) }.asRhythmicSeq.plot;

// .asFrac from PointLib
~factors = (1/[5, 6, 7, 8, 9]).asFrac.multiples(1/4,4);
~page    = { |cell, facts| facts.collect { |fact| [cell[0] * fact.asFloat, cell[1]] }};

(
~plotPage = { arg i;
	var sq = ~page.(~cell[i], ~factors);
	LilyShowableObj.fileName = "~/Desktop/dilations" ++ (i+1);
	sq.asRhythmicSeq.plot;
};
)

~plotPage.(0);

///////////////

(
~base     = "~/Desktop/IEM/POINT/composers/sonja_huber".standardizePath;
~lilyDir  = ~base +/+ "ly";
~template = ~lilyDir +/+ "template1.ly";
~lilypond = "~/bin/lilypond".standardizePath;
~out      = ~base +/+ "out";

~factors  = (1/[5, 6, 7, 8, 9]).asFrac.multiples(1/4,4);
~page     = { |cell, facts| facts.collect { |fact| [cell[0] * fact.asFloat, cell[1]] }};
)

(
~simpleFactor = { arg f; if(f.denom == 1, f.numer, f) };

~write = { arg i;
	var sq, tmp, in, out, cmd, cond, rhy;
    sq  = ~page.(~cell[i], ~factors);
	tmp = File.use(~template, "r", _.readAllString);
	out = ~out +/+ "dilations%".format(i+1);
	in  = out ++ ".ly";
	File.use(in, "w", { arg f;
		f.putString("\\header {\n");
		f.putString("  title = % \"Dilations for Cell No. %\" %\n".format("\\markup { \\fontsize #-1 \\sans", i+1, "}"));
		f.putString("  tagline = \"\"\n");
		f.putString("  subtitle = \" \" % padding the cheesy way\n");
		f.putString("}\n");
		f.putString(tmp);
		sq.do { arg cell, j;
			try {
				f.putString("\\score {\n");
				f.putString("  \\new RhythmicStaff {\n");
				f.putString("    % % %".format("\\set Staff.instrumentName = \\markup { \\char ##x00D7", ~simpleFactor.(~factors[j]), "}\n"));
				rhy = RhythmicCell(cell).string;
				f.putString("    %\n".format(rhy));
				f.putString("  }\n");
				f.putString("}\n");
			} { arg error;
				"Failed to synthesize cell % for factor %".format(i, ~factors[j]).warn;
			};
		};
	});
	cmd = "% -o % %".format(~lilypond, out, in);
	cmd.postln;
	cond = Condition.new;
	cmd.unixCmd	{ arg res;
		if(res == 127, {
			"% failed with code 127".format(~lilypond).warn;
		}, {
			"Ok.".postln;
			"open %.pdf".format(out).unixCmd;
		});
		cond.test = true;
		cond.signal;
	};
	cond;
};
)

// ~write.(0);

fork { ~cell.size.do { arg i; ~write.(i).wait; }}

// ~write.(21);
