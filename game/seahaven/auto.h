struct SolutionLogRec {
    int rank;
    int suit;
    int dest;			// 0-9 = stack, -99 = space, other = king
    SolutionLogRec *next;
};
typedef struct SolutionLogRec *SolutionLog;

extern int AutoPlay();

extern SolutionLog solutionhead;
