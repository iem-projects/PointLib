package at.iem.point.sh.sketches.genetic

case class Settings(info: HeaderInfo, generation: Generation,
                    evaluation: Evaluation, selection: Selection, breeding: Breeding)