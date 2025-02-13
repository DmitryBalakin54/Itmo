const Contract = artifacts.require("Contract");

function sleep(ms) {
  return new Promise(resolve => setTimeout(resolve, ms));
}

contract("Contract", (accounts) => {
  const owner = accounts[0];
  const user = accounts[1];
  const otherUser = accounts[2];

  let instance;

  beforeEach(async () => {
    instance = await Contract.new({ from: owner });
    await instance.depositInitialFunds({ from: owner, value: web3.utils.toWei("5", "ether") });
  });

  afterEach(async () => {
    const contractBalance = await web3.eth.getBalance(instance.address);
    if (contractBalance > 0) {
      await instance.withdrawFunds();
    }
    const finalContractBalance = await web3.eth.getBalance(instance.address);
    assert.equal(finalContractBalance, "0", "Contract should be empty after the test ends: " + finalContractBalance / 1000000000000000000);
  });

  it("should allow the owner to deploy the contract", async () => {
    const contractOwner = await instance.owner();
    assert.equal(contractOwner, owner, "Contract owner is not set correctly");
  });

  it("should allow the user to deposit collateral", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("1", "ether") });
    const collateral = await instance.collateral(user);
    await instance.takeCollateral({from: user});
    assert.equal(collateral.toString(), web3.utils.toWei("1", "ether"), "Collateral is not set correctly");
  });

  it("should allow the user to take a loan", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("3", "ether") });

    const maxLoan = await instance.calculateMaxLoan(user);
    assert.equal(maxLoan.toString(), web3.utils.toWei("4.5", "ether"), "Maximum loan is calculated incorrectly for 150% collateral");

    await instance.takeLoan(web3.utils.toWei("2", "ether"), { from: user });
    const debt = await instance.debt(user);
    
    assert.equal(debt.toString(), web3.utils.toWei("2", "ether"), "Loan amount is not recorded correctly");
    
    await instance.repayLoan({ from: user, value: web3.utils.toWei("2", "ether")});
  });

  it("should allow the user to repay the loan", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("3", "ether") });
    await instance.takeLoan(web3.utils.toWei("2", "ether"), { from: user });
    await instance.repayLoan({ from: user, value: web3.utils.toWei("1", "ether") });
    
    var debt = await instance.debt(user);
    assert.equal(debt.toString(), web3.utils.toWei("1", "ether"), "Loan repayment is not recorded correctly");
    await instance.repayLoan({ from: user, value: web3.utils.toWei("1", "ether") });

    debt = await instance.debt(user);
    assert.equal(debt.toString(), web3.utils.toWei("0", "ether"), "Loan repayment is not recorded correctly");
  });

  it("should correctly calculate the maximum loan based on collateral and liquidation threshold", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("3", "ether") });

    const maxLoan = await instance.calculateMaxLoan(user);
    assert.equal(maxLoan.toString(), web3.utils.toWei("4.5", "ether"), "Maximum loan is calculated incorrectly based on 150% collateral");
    await instance.takeCollateral({from: user});
  });

  it("should allow the owner to liquidate undercollateralized loans after the specified time", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("1.5", "ether") });

    try {
      await instance.takeLoan(web3.utils.toWei("2.3", "ether"), { from: user });
      assert.fail("User should not be able to take a loan exceeding the maximum allowed");
    } catch (error) {
      assert.include(error.message, "Loan amount exceeds collateral value", "Loan amount should not exceed collateral value");
    }

    await instance.takeLoanWithDuration(web3.utils.toWei("1.5", "ether"), 2, { from: user });

    await sleep(3000);
    try {
      await instance.liquidate(user, { from: owner });
      const userCollateral = await instance.collateral(user);
      const userDebtAfterLiquidation = await instance.debt(user);

      assert.equal(userCollateral.toString(), "0", "Collateral was not liquidated");
      assert.equal(userDebtAfterLiquidation.toString(), "0", "Debt was not cleared");
    } catch (error) {
      assert.fail("Liquidation failed: " + error.message);
    }
  });

  it("should not allow the user to take a loan exceeding the collateral", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("1", "ether") });
    try {
      await instance.takeLoan(web3.utils.toWei("2", "ether"), { from: user });
      assert.fail("Expected failure did not occur");
    } catch (error) {
      await instance.takeCollateral({from: user});
      assert(error.message.includes("Loan amount exceeds collateral value"), "Unexpected error");
    }
  });

  it("should not allow the user to take a loan with a duration exceeding the maximum", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("2", "ether") });

    const maxDuration = 730 * 24 * 60 * 60;
    try {
      await instance.takeLoanWithDuration(web3.utils.toWei("1", "ether"), maxDuration + 1, { from: user });
      assert.fail("Expected failure did not occur");
    } catch (error) {
      await instance.takeCollateral({from: user});
      assert(error.message.includes("Duration must be less than 2 years"), error.message);
    }
  });

  it("should allow the user to take a loan with an acceptable duration", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("2", "ether") });
    const loanDuration = 7 * 24 * 60 * 60;

    await instance.takeLoanWithDuration(web3.utils.toWei("1", "ether"), loanDuration, { from: user });
    const debt = await instance.debt(user);
    assert.equal(debt.toString(), web3.utils.toWei("1", "ether"), "Loan amount is not recorded correctly");
    await instance.repayLoan({ from: user, value: web3.utils.toWei("1", "ether")});
  });
  
  it("should allow multiple users to interact with the contract", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("3", "ether") });
    const maxLoanUser1 = await instance.calculateMaxLoan(user);
    assert.equal(maxLoanUser1.toString(), web3.utils.toWei("4.5", "ether"), "Maximum loan for user 1 is calculated incorrectly");
    await instance.takeLoan(web3.utils.toWei("2", "ether"), { from: user });
    const debtUser1 = await instance.debt(user);
    assert.equal(debtUser1.toString(), web3.utils.toWei("2", "ether"), "Loan amount for user 1 is not recorded correctly");
    await instance.repayLoan({ from: user, value: web3.utils.toWei("2", "ether") });

    await instance.depositCollateral({ from: otherUser, value: web3.utils.toWei("2", "ether") });
    const maxLoanUser2 = await instance.calculateMaxLoan(otherUser);
    assert.equal(maxLoanUser2.toString(), web3.utils.toWei("3", "ether"), "Maximum loan for user 2 is calculated incorrectly");
    await instance.takeLoan(web3.utils.toWei("1.5", "ether"), { from: otherUser });
    const debtUser2 = await instance.debt(otherUser);
    assert.equal(debtUser2.toString(), web3.utils.toWei("1.5", "ether"), "Loan amount for user 2 is not recorded correctly");
    await instance.repayLoan({ from: otherUser, value: web3.utils.toWei("1.5", "ether") });

    let debtAfterRepaymentUser1 = await instance.debt(user);
    assert.equal(debtAfterRepaymentUser1.toString(), web3.utils.toWei("0", "ether"), "User 1's debt was not cleared");

    let debtAfterRepaymentUser2 = await instance.debt(otherUser);
    assert.equal(debtAfterRepaymentUser2.toString(), web3.utils.toWei("0", "ether"), "User 2's debt was not fully cleared");
  });
  
  it("should allow the user to update the loan duration", async () => {
    await instance.depositCollateral({ from: user, value: web3.utils.toWei("3", "ether") });
    const initialLoanAmount = web3.utils.toWei("1", "ether");
    const initialDuration = 7 * 24 * 60 * 60;

    await instance.takeLoanWithDuration(initialLoanAmount, initialDuration, { from: user });

    const updatedDuration = 10 * 24 * 60 * 60;
    await instance.takeLoanWithDuration(initialLoanAmount, updatedDuration, { from: user });

    const loanDuration = await instance.loanDuration(user);
    await instance.repayLoan({ from: user, value: web3.utils.toWei("2", "ether")});
    assert.equal(loanDuration.toString(), updatedDuration.toString(), "Loan duration is not updated correctly");
  });
});
