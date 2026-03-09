CREATE TABLE People (
    Id INT PRIMARY KEY,
    FirstName VARCHAR(40) NOT NULL,
    Surname VARCHAR(40) NOT NULL,
    Passport CHAR(10) NOT NULL
);

CREATE TABLE Groups (
    Id INT PRIMARY KEY,
    Name VARCHAR(40) NOT NULL
);

CREATE TABLE Student (
    StudentId INT PRIMARY KEY,
    GroupId INT NOT NULL,
    CONSTRAINT Student_People_FK1 FOREIGN KEY (StudentId) REFERENCES People(Id),
    CONSTRAINT Student_Groups_FK2 FOREIGN KEY (GroupId) REFERENCES Groups(Id)
);

CREATE TABLE Course (
    Id INT PRIMARY KEY,
    Name VARCHAR(40) NOT NULL,
    Description VARCHAR(1000)
);

CREATE TABLE TeacherCourse (
    TeacherId INT NOT NULL,
    CourseId INT NOT NULL,
    PRIMARY KEY (TeacherId, CourseId),
    CONSTRAINT TeacherCourse_People_FK6 FOREIGN KEY (TeacherId) REFERENCES People(Id),
    CONSTRAINT TeacherCourse_Course_FK7 FOREIGN KEY (CourseId) REFERENCES Course(Id)
);

CREATE TABLE GroupsCourse (
    CourseId INT NOT NULL,
    GroupId INT NOT NULL,
    TeacherId INT NOT NULL,
    PRIMARY KEY (CourseId, GroupId),
    CONSTRAINT GroupsCourse_Course_FK3 FOREIGN KEY (CourseId) REFERENCES Course(Id),
    CONSTRAINT GroupsCourse_Groups_FK4 FOREIGN KEY (GroupId) REFERENCES Groups(Id),
    CONSTRAINT GroupsCourse_TeacherCourse_FK5 FOREIGN KEY (TeacherId, CourseId) REFERENCES TeacherCourse(TeacherId, CourseId)
);

CREATE TABLE Club (
    Id INT PRIMARY KEY,
    Name VARCHAR(40) NOT NULL,
    Description VARCHAR(1000),
    TutorId INT NOT NULL,
    CONSTRAINT Club_People_FK8 FOREIGN KEY (TutorId) REFERENCES People(Id)
);

CREATE TABLE Member (
    MemberId INT NOT NULL,
    ClubId INT NOT NULL,
    PRIMARY KEY (MemberId, ClubId),
    CONSTRAINT Member_People_FK9 FOREIGN KEY (MemberId) REFERENCES People(Id),
    CONSTRAINT Member_Club_FK10 FOREIGN KEY (ClubId) REFERENCES Club(Id)
);
