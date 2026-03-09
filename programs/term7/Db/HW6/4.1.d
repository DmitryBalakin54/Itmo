HaveMark(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).

NotPass(StudentName, CourseName) :-
    Students(StudentId, StudentName, GroupId),
    Courses(CourseId, CourseName),
    Plan(GroupId, CourseId, _),
    not HaveMark(StudentId, CourseId).