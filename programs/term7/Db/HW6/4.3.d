HaveMark(StudentId, CourseId) :-
    Marks(StudentId, CourseId, Mark),
    Mark > 2.

HaveCourse(StudentId, CourseId) :-
    Students(StudentId, _, GroupId),
    Plan(GroupId, CourseId, _).

NotPass(StudentName, CourseName) :-
    Students(StudentId, StudentName, GroupId),
    Courses(CourseId, CourseName),
    HaveCourse(StudentId, CourseId),
    not HaveMark(StudentId, CourseId).