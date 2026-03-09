CREATE TABLE Groups (
    GroupId INT PRIMARY KEY,
    GroupName VARCHAR(10) NOT NULL
);

CREATE TABLE Student (
    StudentId INT PRIMARY KEY,
    StudentName VARCHAR(40) NOT NULL,
    GroupId INT NOT NULL,
    CONSTRAINT Student_Groups_FK2 FOREIGN KEY (GroupId) REFERENCES Groups(GroupId)
);

CREATE TABLE Club (
    ClubId INT PRIMARY KEY,
    ClubName VARCHAR(100) NOT NULL,
    ClubStudentHeadId INT NOT NULL,
    CONSTRAINT Club_Student_FK1 FOREIGN KEY (ClubStudentHeadId) REFERENCES Student(StudentId)
);

CREATE TABLE Lecturer (
    LecturerId INT PRIMARY KEY,
    LecturerName VARCHAR(50) NOT NULL
);

CREATE TABLE Course (
    CourseId INT PRIMARY KEY,
    CourseName VARCHAR(60) NOT NULL
);

CREATE TABLE GroupCourse (
    CourseId INT NOT NULL,
    GroupId INT NOT NULL,
    LecturerId INT NOT NULL,
    PRIMARY KEY (CourseId, GroupId),
    CONSTRAINT GroupCourse_Course_KR5 FOREIGN KEY (CourseId) REFERENCES Course(CourseId),
    CONSTRAINT GroupCourse_Groups_FK6 FOREIGN KEY (GroupId) REFERENCES Groups(GroupId),
    CONSTRAINT GroupCourse_Lecturer_KR7 FOREIGN KEY (LecturerId) REFERENCES Lecturer(LecturerId)
);

CREATE TABLE Mark (
    Mark CHAR NOT NULL,
    StudentId INT NOT NULL,
    CourseId INT NOT NULL,
    PRIMARY KEY (StudentId, CourseId),
    CONSTRAINT Mark_Student_FK3 FOREIGN KEY (StudentId) REFERENCES Student(StudentId),
    CONSTRAINT Mark_Course_FK4 FOREIGN KEY  (CourseId) REFERENCES Course(CourseId)
);
