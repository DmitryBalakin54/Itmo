HaveCourse(StudentId) :-
    Students(StudentId, _, GroupId),
    Groups(GroupId, _),
    Plan(GroupId, CourseId, _),
    CourseId = :CourseId.

HaveMarkByCourseId(StudentId) :-
    Students(StudentId, _, _),
    Marks(StudentId, CourseId, _),
    CourseId = :CourseId.

StudentsByHaventMarkByCourseId(StudentId, StudentName, GroupName) :-
    Students(StudentId, StudentName, GroupId),
    Groups(GroupId, GroupName),
    not HaveMarkByCourseId(StudentId),
    HaveCourse(StudentId).