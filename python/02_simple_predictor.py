import numpy as np
import matplotlib.pyplot as plt


class SimplePredictor:
    def __init__(self, num_inputs, learning_rate=0.01, epochs=100):
        self.num_inputs = num_inputs
        self.learning_rate = learning_rate
        self.epochs = epochs
        self.weights = np.random.rand(num_inputs)

    def predict(self, inputs):
        return np.dot(inputs, self.weights)

    def train(self, training_inputs, training_outputs):
        Z1 = []
        Z2 = []
        for epoch in range(self.epochs):
            for i in range(len(training_inputs)):
                prediction = self.predict(training_inputs[i])
                error = training_outputs[i] - prediction
                self.weights += self.learning_rate * error * training_inputs[i]
                Z1.append(self.weights[0])
                Z2.append(self.weights[1])
                plt.scatter(Z1, Z2, c='blue', alpha=0.5)
                plt.xlabel('Weight 1')
                plt.ylabel('Weight 2')
                plt.title('Weight Evolution During Training')
                plt.scatter(self.weights[0], self.weights[1], c='red')
                print("final weights:", self.weights)
                plt.show()


# sample dataset
training_inputs = np.array([[0, 0],
                            [0, 1],
                            [1, 0],
                            [1, 1]])
training_outputs = np.array([0, 1, 1, 0])  # XOR problem
SimplePredictor_instance = SimplePredictor(
    num_inputs=2, learning_rate=0.1, epochs=10)
SimplePredictor_instance.train(training_inputs, training_outputs)

input_test = np.array([1, 0])
output_test = np.array([1])

print("Testing the model:")
for i in range(len(input_test)):
    prediction = SimplePredictor_instance.predict(input_test)
    print(
        f"Input: {input_test}, Predicted Output: {prediction}, Actual Output: {output_test}")
