// SPDX-License-Identifier: MIT
pragma solidity ^0.8.0;

contract Contract {
    address public owner;
    uint256 public liquidationThreshold = 150;
    uint256 public defaultDuration = 7 days;
    uint256 public maxDuration = 730 days;
    uint256 private ownersMoney = 0;

    mapping(address => uint256) public collateral;
    mapping(address => uint256) public debt;
    mapping(address => uint256) public loanTimestamp; 
    mapping(address => uint256) public loanDuration;
    mapping(address => uint256) public minLoanDuration;

    event CollateralDeposited(address indexed user, uint256 amount);
    event LoanTaken(address indexed user, uint256 amount, uint256 duration);
    event LoanRepaid(address indexed user, uint256 amount);
    event CollateralLiquidated(address indexed user, uint256 amount);
    event CollateralReturned(address indexed user, uint256 amount);
    event CollateralReturneduser(address indexed user);


    modifier onlyOwner() {
        require(msg.sender == owner, "Only owner can call this function");
        _;
    }

    modifier notOwner() {
        require(msg.sender != owner, "Only owner can call this function");
        _;
    }

    constructor() {
        owner = msg.sender;
    }

    function depositInitialFunds() public onlyOwner payable {
        require(msg.value > 0, "Owner must deposit initial funds");
        ownersMoney += msg.value;
    }

    function withdrawFunds() external onlyOwner {
        uint balance = address(this).balance;
        require(balance > 0, "No funds to withdraw");
        payable(owner).transfer(ownersMoney);
        ownersMoney = 0;
    }


    function depositCollateral() public payable notOwner {
        require(msg.value > 0, "Collateral must be greater than 0");
        collateral[msg.sender] += msg.value;

        emit CollateralDeposited(msg.sender, msg.value);
    }

    function calculateMaxLoan(address user) public view returns (uint256) {
        return (collateral[user] * liquidationThreshold) / 100;
    }

    function newDuration(uint256 newV, uint256 oldV) private returns (uint256) {
        uint256 minV = minLoanDuration[msg.sender];
        if (oldV == 0) {
            return newV;
        }

        if (newV <= oldV) {
            return newV;
        }

        if (newV <= oldV + minV / 2) {
            minLoanDuration[msg.sender] = minV / 2;
            return newV;
        }

        return oldV;
    }
    
    function takeLoanWithDuration(uint256 amount, uint256 duration) public notOwner {
        uint256 maxLoan = calculateMaxLoan(msg.sender);
        require(duration < maxDuration, "Duration must be less than 2 years");
        require(amount > 0, "Loan amount must be greater than 0");
        require(amount <= maxLoan, "Loan amount exceeds collateral value");
        require(amount <= ownersMoney, "No enugh money");
        require(duration > 0, "Duration must be greater than 0");
        if (minLoanDuration[msg.sender] > 0) {
            require(duration <= minLoanDuration[msg.sender] / 2 + loanDuration[msg.sender], "Duration must be less");
        }

        debt[msg.sender] += amount;

        if (loanTimestamp[msg.sender] == 0) {
            loanTimestamp[msg.sender] = block.timestamp;
        }

        if (minLoanDuration[msg.sender] == 0) {
            minLoanDuration[msg.sender] = duration;
        } 

        loanDuration[msg.sender] = newDuration(duration, loanDuration[msg.sender]);
        payable(msg.sender).transfer(amount);
        ownersMoney -= amount;

        emit LoanTaken(msg.sender, amount, duration);
    }

    function takeLoan(uint256 amount) public notOwner {
        uint256 duration = 0;

        if (loanDuration[msg.sender] == 0) {
            duration = defaultDuration;
        } else {
            duration = loanDuration[msg.sender];
        }

        takeLoanWithDuration(amount, duration);
    }

    function repayLoan() public payable notOwner {
        require(block.timestamp <= loanTimestamp[msg.sender] + loanDuration[msg.sender], "Loan duration passed yet "); 
        require(debt[msg.sender] > 0, "User has no debt");
        require(msg.value > 0, "Repayment must be greater than 0");
        require(msg.value <= debt[msg.sender], "Repayment exceeds debt");

        debt[msg.sender] -= msg.value;
        ownersMoney += msg.value;

        if (debt[msg.sender] == 0) {
            uint256 userCollateral = collateral[msg.sender];
            collateral[msg.sender] = 0;
            minLoanDuration[msg.sender] = 0;
            loanDuration[msg.sender] = 0;
            loanTimestamp[msg.sender] = 0;
            payable(msg.sender).transfer(userCollateral); 

            emit CollateralReturned(msg.sender, userCollateral);
        }

        emit LoanRepaid(msg.sender, msg.value);
    }

    function takeCollateral() public payable notOwner {
        require(debt[msg.sender] == 0, "User has debt");
        require(collateral[msg.sender] > 0, "User hasn`t collateral");

        payable(msg.sender).transfer(collateral[msg.sender]);
        collateral[msg.sender] = 0;

        emit CollateralReturneduser(msg.sender);
    }
    

    function liquidate(address user) public onlyOwner {
        require(block.timestamp > loanTimestamp[user] + loanDuration[user], "Loan duration not passed yet"); 

        uint256 userCollateral = collateral[user];
        collateral[user] = 0;
        debt[user] = 0;
        minLoanDuration[msg.sender] = 0;
        loanDuration[msg.sender] = 0;
        loanTimestamp[msg.sender] = 0;

        payable(owner).transfer(userCollateral);

        emit CollateralLiquidated(user, userCollateral);
    }
}
