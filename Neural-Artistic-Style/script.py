import tensorflow as tf
import numpy as np
import scipy
from scipy.io import loadmat

from PIL import Image

import imageio
import pdb

import os

# Image dimensions constants. 
OUT_WIDTH = 700
OUT_HEIGHT = 500
OUT_CHANNELS = 3
MEAN_VALUES = np.array([123.68, 116.779, 103.939]).reshape((1,1,1,3))

VGG_PATH = "pretrained_models/imagenet-vgg-verydeep-19.mat"
# load the Pre-trained VGG weights
vgg = loadmat(VGG_PATH)
# isolate the layers - and not the metadata
layers = vgg['layers'][0]

# encapsulate the functions needed to load the VGG model in a class
class VGGLoader:
    
    def __init__(self, vgg_path, out_height, out_width, out_channels):
        self.vgg_path = vgg_path
        self.out_height = out_height
        self.out_width = out_width
        self.out_channels = out_channels
        
    # load the model as a matrix
    # store the pre-trained weights as a class atribute
    def _load_layers(self):
        # load the VGG model
        vgg = loadmat(self.vgg_path)
        # return just the pre-trained weights
        self.layers =  vgg['layers'][0]
    
    # Load the weights of a given layer
    def _get_weights(self,layer):
        # load pre-trained weights and biases
        weight = self.layers[layer][0][0][2][0][0]
        bias = self.layers[layer][0][0][2][0][1]
        return weight, bias
    
    # create a tensorflow convolutional layer based on the pre-trained weights
    def _conv2d(self, prev_layer, layer):
    
        weight, bias = self._get_weights(layer)
        W = tf.constant(weight)
        b = tf.constant(np.reshape(bias, (bias.size)))
        return tf.nn.conv2d(
            prev_layer, filter=W, strides=[1, 1, 1, 1], padding='SAME') + b
    
    # apply a relu activation onto a Conv2d Layer
    def _relu(self, conv2d_layer):
        return tf.nn.relu(conv2d_layer)
    
    # create a convolutional layer on the pretrained weights, then apply an activation
    def _conv2d_relu(self, prev_layer, layer):
        return self._relu(self._conv2d(prev_layer, layer))

    # create an average-pooling layer
    def _avgpool(self, prev_layer):
        return tf.nn.avg_pool(prev_layer, ksize=[1, 2, 2, 1], strides=[1, 2, 2, 1], padding='SAME')
    
    
    ########################################
    # Key method - Load and return the VGG model as a Tensorflow Model
    ########################################
    def get_model(self):
        # Load and save the pre-trained weights
        self._load_layers()
        
        # build up the sequential model
        model = {}
        model['input']   = tf.Variable(np.zeros((1, self.out_height, self.out_width, \
                                                 self.out_channels)), dtype = 'float32')
        model['conv1_1']  = self._conv2d_relu(model['input'], 0)
        model['conv1_2']  = self._conv2d_relu(model['conv1_1'], 2)
        model['avgpool1'] = self._avgpool(model['conv1_2'])
        model['conv2_1']  = self._conv2d_relu(model['avgpool1'], 5)
        model['conv2_2']  = self._conv2d_relu(model['conv2_1'], 7)
        model['avgpool2'] = self._avgpool(model['conv2_2'])
        model['conv3_1']  = self._conv2d_relu(model['avgpool2'], 10)
        model['conv3_2']  = self._conv2d_relu(model['conv3_1'], 12)
        model['conv3_3']  = self._conv2d_relu(model['conv3_2'], 14)
        model['conv3_4']  = self._conv2d_relu(model['conv3_3'], 16)
        model['avgpool3'] = self._avgpool(model['conv3_4'])
        model['conv4_1']  = self._conv2d_relu(model['avgpool3'], 19)
        model['conv4_2']  = self._conv2d_relu(model['conv4_1'], 21)
        model['conv4_3']  = self._conv2d_relu(model['conv4_2'], 23)
        model['conv4_4']  = self._conv2d_relu(model['conv4_3'], 25)
        model['avgpool4'] = self._avgpool(model['conv4_4'])
        model['conv5_1']  = self._conv2d_relu(model['avgpool4'], 28)
        model['conv5_2']  = self._conv2d_relu(model['conv5_1'], 30)
        model['conv5_3']  = self._conv2d_relu(model['conv5_2'], 32)
        model['conv5_4']  = self._conv2d_relu(model['conv5_3'], 34)
        model['avgpool5'] = self._avgpool(model['conv5_4'])
        return model

def content_loss_func(sess, model):
    """
    Content loss function as defined in the paper.
    """
    def _content_loss(p, x):
        # N is the number of filters (at layer l).
        N = p.shape[3]
        # M is the height times the width of the feature map (at layer l).
        M = p.shape[1] * p.shape[2]
        return (1 / (4 * N * M)) * tf.reduce_sum(tf.pow(x - p, 2))
    return _content_loss(sess.run(model['conv4_2']), model['conv4_2'])

# weights to apply to the different layers
STYLE_LAYERS = [
    ('conv1_1', 0.5),
    ('conv2_1', 1.0),
    ('conv3_1', 1.5),
    ('conv4_1', 3.0),
    ('conv5_1', 4.0),
]

def style_loss_func(sess, model):
    """
    Style loss function as defined in the paper.
    """
    def _gram_matrix(F, N, M):
        """
        The gram matrix G.
        """
        Ft = tf.reshape(F, (M, N))
        return tf.matmul(tf.transpose(Ft), Ft)

    def _style_loss(a, x):
        """
        The style loss calculation.
        """
        # N is the number of filters (at layer l).
        N = a.shape[3]
        # M is the height times the width of the feature map (at layer l).
        M = a.shape[1] * a.shape[2]
        # A is the style representation of the original image (at layer l).
        A = _gram_matrix(a, N, M)
        # G is the style representation of the generated image (at layer l).
        G = _gram_matrix(x, N, M)
        result = (1 / (4 * N**2 * M**2)) * tf.reduce_sum(tf.pow(G - A, 2))
        return result

    E = [_style_loss(sess.run(model[layer_name]), model[layer_name]) for layer_name, _ in STYLE_LAYERS]
    W = [w for _, w in STYLE_LAYERS]
    loss = sum([W[l] * E[l] for l in range(len(STYLE_LAYERS))])
    return loss

def generate_noise_image(content_image, noise_ratio):
    """
    Returns a noise image intermixed with the content image at a certain ratio.
    """
    noise_image = np.random.uniform(
            -20, 20,
            (1, OUT_HEIGHT,OUT_WIDTH, OUT_CHANNELS)).astype('float32')
    # White noise image from the content representation. Take a weighted average
    # of the values
    input_image = noise_image * noise_ratio + content_image * (1 - noise_ratio)
    return input_image

def load_image(path):
    image = Image.open(path)
    # resize image to inputted size
    image = image.resize((OUT_WIDTH, OUT_HEIGHT))
    # reshape image to appropriate 
    # Resize the image for convnet input, there is no change but just
    # add an extra dimension.
    image_data = np.array(image.getdata()).reshape(( OUT_HEIGHT, OUT_WIDTH, OUT_CHANNELS))
    image_data= np.reshape(image_data, ((1,) + image_data.shape))
    # Input to the VGG model expects the mean to be subtracted.
    image_data = image_data - MEAN_VALUES
    return image, image_data, 

def save_image(path, image):
    # Output should add back the mean.
    image = image + MEAN_VALUES
    # Get rid of the first useless dimension, what remains is the image.
    image = image[0]
    image = np.clip(image, 0, 255).astype('uint8')
    imageio.imwrite(path, image)

sess = tf.InteractiveSession()

CONTENT_IMAGE = "images/coat.jpg"
STYLE_IMAGE = "images/picasso.jpg"

content_image, content_data = load_image(CONTENT_IMAGE)
style_image, style_data = load_image(STYLE_IMAGE)


input_image = generate_noise_image(content_data, .6)


loader = VGGLoader(out_channels=OUT_CHANNELS, out_height=OUT_HEIGHT, out_width= OUT_WIDTH, vgg_path= VGG_PATH)

model = loader.get_model()

# Construct content_loss using content_image.
sess.run(model['input'].assign(content_data))
content_loss = content_loss_func(sess, model)

# Construct style_loss using style_image.
sess.run(model['input'].assign(style_data))
style_loss = style_loss_func(sess, model)

# Constant to put more emphasis on content loss.
BETA = 5
# Constant to put more emphasis on style loss.
ALPHA = 100

total_loss = BETA * content_loss + ALPHA * style_loss


# The content is built from one layer, while the style is from five
# layers. Then we minimize the total_loss, which is the equation 7.
optimizer = tf.train.AdamOptimizer(2.0)
train_step = optimizer.minimize(total_loss)

sess.run(tf.global_variables_initializer())
sess.run(model['input'].assign(input_image))

# Number of iterations to run.
ITERATIONS = 1000  

sess.run(tf.initialize_all_variables())
sess.run(model['input'].assign(input_image))
for it in range(ITERATIONS):
    sess.run(train_step)
    if it%20 == 0:
        # Print every 100 iteration.
        mixed_image = sess.run(model['input'])
        print('Iteration %d' % (it))
        print('sum : ', sess.run(tf.reduce_sum(mixed_image)))
        print('cost: ', sess.run(total_loss))

        if not os.path.exists("output"):
            os.mkdir("output")

        filename = 'output/%d.png' % (it)
        save_image(filename, mixed_image)


