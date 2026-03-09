HaveCourse(StudentId, CourseId) :-
    Students(StudentId, _, GroupId),
    Plan(GroupId, CourseId, _).
HaveCourse(StudentId, CourseId) :-
    Marks(StudentId, CourseId, _).