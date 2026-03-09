HaveNegMark(StudentId, CourseId) :-
    Marks(StudentId, CourseId, Mark),
    Mark <= 2.

NotPass(StudentName, CourseName) :-
    Students(StudentId, StudentName, GroupId),
    Courses(CourseId, CourseName),
    Plan(GroupId, CourseId, _),
    HaveNegMark(StudentId, CourseId).