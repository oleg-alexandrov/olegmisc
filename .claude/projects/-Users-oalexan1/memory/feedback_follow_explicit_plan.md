---
name: feedback_follow_explicit_plan
description: "When Oleg gives an explicit directive/plan, execute it; don't re-propose alternatives he already weighed."
metadata: 
  node_type: memory
  type: feedback
  originSessionId: 1669b585-4fd9-4f3e-b171-1333bb4aa1ef
---

When Oleg has laid out an explicit plan or directive, follow it and proceed — do not pause to re-propose an alternative he has effectively already considered. He will say "I think I made it clear enough" / "go boy" when this happens.

**Why:** He often has context the alternative doesn't account for (e.g. on the linux-arm ASP build he insisted on the MultiView feedstock breakup over an inline build because "we will have to redo the full mac and linux builds too and break up will help" — a cross-platform payoff beyond the immediate task).

**How to apply:** Surface a genuine blocker or a fact that changes the premise, briefly, once. But if it's just a cheaper/faster alternative and he's already given a clear directive, execute the directive. Reserve AskUserQuestion for true ambiguity, not for re-litigating decided plans. See [[feedback_precise_instructions]].
