program main
  use mod_print_utils, only: print_matrix
  use cls_DenseLayer, only: DenseLayer
  use mod_math_utils, only: rand_normal_mat, rand_normal_arr
  use cls_Accuracy, only: Accuracy
  use cls_Loss, only: Loss
  use cls_ReluActivator, only: ReluActivator
  use cls_SoftmaxActivator, only: SoftmaxActivator
  use csv_module

  ! Status code variables - Use for error handling
  integer :: status_code
  logical :: status_ok

  ! CSV read/write items
  type(csv_file) :: csv
  character(:), allocatable ::  csv_path
  real(kind=8), dimension(:), allocatable :: csv_read_output

  ! Data and y_true. y_true is 1-dimensional (not one-hot)
  real(kind=8), dimension(:,:), allocatable :: x
  integer, dimension(:), allocatable :: y_true

  ! Neural network layers, activators
  type(DenseLayer) :: layer1
  type(ReluActivator) :: activator1
  type(DenseLayer) :: layer2
  type(SoftmaxActivator) :: activator2

  ! Neural network metrics
  type(Loss) :: lossCalc
  type(Accuracy) :: accuracyCalc
  real(kind=8), dimension(:,:), allocatable :: bestL1Weights
  real(kind=8), dimension(:), allocatable :: bestL1Biases
  real(kind=8), dimension(:,:), allocatable :: bestL2Weights
  real(kind=8), dimension(:), allocatable :: bestL2Biases
  real(kind=8) :: bestLoss, bestAccuracy
  real(kind=8) :: learningRate

  ! Control variables
  integer :: i
  logical :: dumb_algo ! If false, use backpropogation - used for comparison

  dumb_algo = .false.
  learningRate = .0001


  ! Read dataset and set up
  csv_path = "X.csv"
  allocate(x(300,2))

  call csv%read(csv_path, status_ok=status_ok)
  call check_status(status_ok, "read x")

  call csv%get(1, csv_read_output, status_ok)
  x(:,1) = csv_read_output
  call csv%get(2, csv_read_output, status_ok)
  x(:,2) = csv_read_output

  csv_path = "y.csv"
  call csv%read(csv_path, status_ok=status_ok)
  call check_status(status_ok, "read y")
  allocate(y_true(300))
  call csv%get(1, y_true, status_ok)
  y_true = y_true + 1




  if (dumb_algo) then
    call layer1%init(2, 3)
    bestL1Weights = layer1%getWeights()
    bestL1Biases = layer1%getBiases()

    call layer2%init(3, 3)
    bestL2Weights = layer2%getWeights()
    bestL2Biases = layer2%getBiases()

    bestLoss = huge(0.0)
    bestAccuracy = 0.0

    do i = 1, 10000
      call layer1%setWeights(bestL1Weights +&
        0.05 * rand_normal_mat(size(bestL1Weights, 1), size(bestL1weights, 2)))
      call layer1%setBiases(&
        bestL1Biases + 0.05 * rand_normal_arr(size(bestL1Biases)))
      call layer2%setWeights(bestL2Weights +&
        0.05 * rand_normal_mat(size(bestL2Weights, 1), size(bestL2weights, 2)))
      call layer2%setBiases(&
        bestL2Biases + 0.05 * rand_normal_arr(size(bestL2Biases)))

      call layer1%forward(x)
      call activator1%forward(layer1)
      call layer2%forward(activator1%getOutputs())
      call activator2%forward(layer2)

      call lossCalc%calculateLoss(activator2%getOutputs(), y_true)
      call accuracyCalc%calculate(activator2%getOutputs(), y_true)

      if (lossCalc%getLoss() < bestLoss) then
        bestLoss = lossCalc%getLoss()
        bestAccuracy = accuracyCalc%getAccuracy()
        bestL1Weights = layer1%getWeights()
        bestL1Biases = layer1%getBiases()
        bestL2Weights = layer2%getWeights()
        bestL2Biases = layer2%getBiases()
        print *, "New weights and biases found"
        print *, "Iteration: "
        print *, i
        print *, "Loss: "
        print *, lossCalc%getLoss()
        print *, "Accuracy: "
        print *, accuracyCalc%getAccuracy()
      else
        call layer1%setWeights(bestL1Weights)
        call layer1%setBiases(bestL1Biases)
        call layer2%setWeights(bestL2Weights)
        call layer2%setBiases(bestL2Biases)
      end if
    end do

    print *, "Best Layer 1 Biases:"
    print *, bestL1Biases
    print *, "Best Layer 1 Weights:"
    call print_matrix(bestL1Weights)
    print *, "Best Layer 2 Biases:"
    print *, bestL2Biases
    print *, "Best Layer 2 Weights:"
    call print_matrix(bestL2Weights)

    print *, "Best Loss achieved: "
    print *, bestLoss
    print *, "Best Accuracy achieved:"
    print *, bestAccuracy
  else
    call layer1%init(2, 3)

    call layer2%init(3, 3)
                              ! TODO: Add feedback stuff and test 
    do i = 1, 100
      call layer1%forward(x)
      call activator1%forward(layer1)
      call layer2%forward(activator1%getOutputs())
      call activator2%forward(layer2)
      call lossCalc%calculateLoss(activator2%getOutputs(), y_true)
      call accuracyCalc%calculate(activator2%getOutputs(), y_true)

      ! go backwards
      call lossCalc%calcDLossDSoftmaxCombined(activator2%getOutputs(), y_true)
      call activator2%backward(lossCalc%getDLoss())
      call layer2%backward(activator2%getDInputs())
      call activator1%backward(layer2%getDInputs())
      call layer1%backward(activator1%getDInputs())

      ! update the weights
      call layer2%setWeights(&
        layer2%getWeights() - learningRate * layer2%getDWeights())
      call layer2%setBiases(&
        layer2%getBiases() - learningRate * layer2%getDBiases())
      call layer1%setWeights(&
        layer1%getWeights() - learningRate * layer1%getDWeights())
      call layer1%setBiases(&
        layer1%getBiases() - learningRate * layer1%getDBiases())
    end do

    print *, "Final Layer 1 Biases:"
    print *, layer1%getBiases()
    print *, "Final Layer 1 Weights:"
    call print_matrix(layer1%getWeights())

    print *, "Final Layer 2 Biases:"
    print *, layer2%getBiases()
    print *, "Final Layer 2 Weights:"
    call print_matrix(layer2%getWeights())

    print *, "Final Loss:"
    print *, lossCalc%getLoss()
    print *, "Final Accuracy:"
    print *, accuracyCalc%getAccuracy()
  end if


contains
  subroutine check_status(status, marker)
    logical :: status
    character(*) :: marker
    if (.not.status) then
      print *, "A status code indicating a failure occurred at: " // marker
      STOP
    end if
  end subroutine check_status

  subroutine check_status_int(status, marker)
    integer :: status
    character(*) :: marker
    if(status.ne.0) then
      print *, "A status code indicating a failure occurred at: " // marker
      STOP
    end if
  end subroutine check_status_int
end program main
